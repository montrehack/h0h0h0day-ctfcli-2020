#!/usr/bin/python3


import socketserver
import threading
import hashlib
import base64
import time
from helper import decrypt


expected = "267373db136a3009cc4d5e9c5bebdaf784a7b6417f395867f62f1a17409c11de406afcd8f90999b6e0eab87e76cf1dfb9c19e80d2ef4c53436bc7242f60afa0f"
not_really_expected = "8729a835b911bb3b8e75e2d2f204ec1e0f47ca1ae297a68f346f2c42035ebeb60da714407f4661bee7f87a64c4108da6ffc05f6d37e5756fa9eef053ad13ea42"
hint = "MjE4MDk4NDIxOTA4NDIxMIlawYHA88kkgkKUO7HXG/KKr8+kmNtD+QfqWkadvV+Z+LJNgbNLv8FX3Z6u8+4LXg=="
success = "MjE4MDk4NDIxOTA4NDIxMAn7TWpuV5LKkeYZCViA/4Ww/GyVvCzC+Sq1KLEGUqq3"

class ThreadedTCPRequestHandler(socketserver.BaseRequestHandler):
    def handle(self):
        print(f'Connected {self.client_address[0]}')
        self.print_logo()
        self.ask_for_password()
        while not self.test_pwd(self.read_from_socket()):
            self.ask_for_password()

    def test_pwd(self, pwd):
            hashed_pwd = self.hash(pwd)
            print(f'hash : {hashed_pwd}')
            if hashed_pwd == expected:
                self.print_success()
                self.my_print(decrypt(success, pwd))
                self.my_print("https://www.youtube.com/watch?v=g5K0COTiKec&ab_channel=NapalmRecords")
                return True
            elif hashed_pwd == not_really_expected:
                self.my_print(decrypt(hint, pwd))
            return False

    def hash(self, s):
        stripped = str(s).encode("utf-8")
        print(f'Testing password : {stripped}')
        return hashlib.sha512(stripped).hexdigest()

    def print_logo(self):
        self.my_print("")
        self.my_print("***************************************************")
        self.my_print("     Undead unicorns administration panel")
        self.my_print("***************************************************")
        self.my_print("")
        self.my_print("          _______\)%%%%%%%%._               ")
        self.my_print("         `''''-'-;   % % % % %'-._          ")
        self.my_print("                 :b) \            '-.       ")
        self.my_print("                 : :__)'    .'    .'        ")
        self.my_print("                 :.::/  '.'   .'            ")
        self.my_print("                 o_i/   :    ;              ")
        self.my_print("                        :   .'              ")
        self.my_print("                         ''`                ")
        self.my_print("")
        self.my_print("Plan :      Invasion of Dundee")
        self.my_print("Status :    Enabled")
        self.my_print("ETA :       2 days")
        self.my_print("Requester : The great master of evil")

    def my_print(self, s):
          self.request.sendall((f'{s}\r\n').encode())

    def ask_for_password(self):
        self.my_print("")
        self.my_print("Enter your password to cancel the invasion :")

    def read_from_socket(self):
        return str(self.request.recv(1024), "ascii").rstrip("\n")

    def print_success(self):
        time.sleep(0.5)
        self.my_print("Identity : Zargotrax")
        time.sleep(0.5)
        self.my_print("> kill invasion_of_dundee")
        time.sleep(0.5)
        self.my_print("...")
        time.sleep(0.5)
        self.my_print("...")
        time.sleep(0.5)
        self.my_print("...")
        time.sleep(0.5)
        self.my_print("> release_mind_control --target unicorns")
        time.sleep(0.5)
        self.my_print("...")
        time.sleep(0.5)
        self.my_print("...")
        time.sleep(0.5)
        self.my_print("...")
        time.sleep(0.5)
        self.my_print("> recal_troops")
        self.my_print("...")
        time.sleep(0.5)
        self.my_print("...")
        time.sleep(0.5)
        self.my_print("...")
        time.sleep(0.5)
        self.my_print("Invasion plan : Cancelled")
        self.my_print("")

class ThreadedTCPServer(socketserver.ThreadingMixIn, socketserver.TCPServer):
    pass

if __name__ == "__main__":
    HOST, PORT = "0.0.0.0", 27777

    server = ThreadedTCPServer((HOST, PORT), ThreadedTCPRequestHandler)
    # Create the server, binding to outside on port 27777
    with server:
        server_thread = threading.Thread(target=server.serve_forever)

        server_thread.daemon = True
        server_thread.start()

        while not input("\nPress q to stop server") == "q":
            pass

        server.server_close()
