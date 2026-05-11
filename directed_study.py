#!/usr/bin/env python3
"""
dispense_web.py  (SEPARATE standalone dispenser controller)

What it does:
- Runs a tiny web server with an input box for *integer liters*
- When you submit, it sends:  "dispense,<liters>\n"  to the Arduino over TCP
- Optional buttons: STOP, STATUS

Default Arduino target (from your serial output):
  192.168.1.142:4080

Run:
  python3 dispense_web.py
Then open:
  http://localhost:8080
"""

import socket
import urllib.parse
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import argparse
import threading
from typing import Dict, Tuple, Optional

# ---------- TCP client to Arduino ----------
arduino_lock = threading.Lock()

def tcp_send(host: str, port: int, line: str, timeout: float = 3.0) -> str:
    """Send one newline-terminated command line and return one reply line (if any)."""
    data = (line.rstrip("\n") + "\n").encode("utf-8")
    with arduino_lock:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.settimeout(timeout)
        s.connect((host, port))
        s.sendall(data)
        try:
            resp = s.recv(1024).decode("utf-8", errors="ignore").strip()
        except Exception:
            resp = ""
        s.close()
    return resp

# ---------- HTTP handler ----------
PAGE_TEMPLATE = """<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <title>Dispenser Control</title>
    <style>
      body {{ font-family: Arial, sans-serif; margin: 40px; }}
      .card {{ border:1px solid #ddd; border-radius:12px; padding:18px; max-width:480px; }}
      input, button {{ font-size:16px; padding:10px; }}
      .row {{ margin: 10px 0; }}
      .msg {{ margin-top:14px; padding:12px; background:#f5f5f5; border-radius:10px; }}
      small {{ color:#666; }}
    </style>
  </head>
  <body>
    <div class="card">
      <h2>Dispense Water</h2>
      <div class="row">
        <form method="GET" action="/dispense">
          <label>Liters (integer):</label><br><br>
          <input name="liters" type="number" step="1" min="0" placeholder="e.g., 1" required>
          <button type="submit">Dispense</button>
        </form>
      </div>

      <div class="row">
        <form method="GET" action="/status" style="display:inline;">
          <button type="submit">Status</button>
        </form>
        <form method="GET" action="/stop" style="display:inline; margin-left:10px;">
          <button type="submit">STOP</button>
        </form>
      </div>

      <small>Arduino: {arduino_host}:{arduino_port}</small>

      {message}
    </div>
  </body>
</html>
"""

class Handler(BaseHTTPRequestHandler):
    # These get injected at server start
    arduino_host = None
    arduino_port = None

    def do_GET(self):
        parsed = urllib.parse.urlparse(self.path)
        path = parsed.path
        qs = urllib.parse.parse_qs(parsed.query)

        msg_html = ""

        if path == "/":
            msg_html = ""
            return self._send_page(msg_html)

        if path == "/dispense":
            liters_str = (qs.get("liters", [""])[0] or "").strip()
            try:
                # accept any integer liters (0 allowed, but you can change to min 1 if you want)
                liters = int(float(liters_str))  # allows "2.0" → 2
                if liters < 0:
                    raise ValueError("liters must be >= 0")

                resp = tcp_send(self.arduino_host, self.arduino_port, f"dispense,{liters}")
                msg_html = f"<div class='msg'><b>Sent:</b> dispense,{liters}<br><b>Arduino:</b> {resp or '(no response)'}</div>"
                return self._send_page(msg_html)

            except Exception:
                msg_html = "<div class='msg'><b>Error:</b> invalid liters. Please enter an integer ≥ 0.</div>"
                return self._send_page(msg_html, status=400)

        if path == "/stop":
            try:
                resp = tcp_send(self.arduino_host, self.arduino_port, "stop")
                msg_html = f"<div class='msg'><b>Sent:</b> stop<br><b>Arduino:</b> {resp or '(no response)'}</div>"
                return self._send_page(msg_html)
            except Exception as e:
                msg_html = "<div class='msg'><b>Error:</b> could not send stop.</div>"
                return self._send_page(msg_html, status=500)

        if path == "/status":
            try:
                resp = tcp_send(self.arduino_host, self.arduino_port, "status")
                msg_html = f"<div class='msg'><b>Sent:</b> status<br><b>Arduino:</b> {resp or '(no response)'}</div>"
                return self._send_page(msg_html)
            except Exception:
                msg_html = "<div class='msg'><b>Error:</b> could not request status.</div>"
                return self._send_page(msg_html, status=500)

        return self._send_text(404, "Not found")

    def _send_page(self, message_html: str, status: int = 200):
        html = PAGE_TEMPLATE.format(
            arduino_host=self.arduino_host,
            arduino_port=self.arduino_port,
            message=message_html
        )
        data = html.encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "text/html; charset=utf-8")
        self.send_header("Content-Length", str(len(data)))
        self.send_header("Connection", "close")
        self.end_headers()
        self.wfile.write(data)

    def _send_text(self, status: int, text: str):
        data = text.encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "text/plain; charset=utf-8")
        self.send_header("Content-Length", str(len(data)))
        self.send_header("Connection", "close")
        self.end_headers()
        self.wfile.write(data)

    def log_message(self, fmt, *args):
        # quiet default logging; comment out if you want request logs
        return


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--arduino-ip", default="192.168.1.142")
    parser.add_argument("--arduino-port", type=int, default=4080)
    parser.add_argument("--web-port", type=int, default=8080)
    args = parser.parse_args()

    Handler.arduino_host = args.arduino_ip
    Handler.arduino_port = args.arduino_port

    server = ThreadingHTTPServer(("0.0.0.0", args.web_port), Handler)
    print(f"Web UI: http://localhost:{args.web_port}")
    print(f"Arduino target: {args.arduino_ip}:{args.arduino_port}")
    server.serve_forever()


if __name__ == "__main__":
    main()
