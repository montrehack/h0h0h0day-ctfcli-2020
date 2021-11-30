#!/usr/bin/env python3
import H3S
import signal

def prompt():
    with open("static/prompt") as f:
        for line in f:
            print(line, end="")

def secret():
    return H3S.file_to_code_array("static/secret.h3s")

def checkinput(user_input):
    return all(c in "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 " for c in user_input)

def main():
    prompt()
    code = []
    while True:
        tmp = input()
        if (tmp == "secret"):
            code = secret()
            break
        elif (tmp == ""):
            break
        elif (checkinput(tmp)):
            code.append(tmp)
        else:
            print("ERR: illegal character detected. please refer to the end user manual")
            exit(1)

    vm = H3S.VM()
    print("Parsing your code...")
    vm.lex(code)
    print("\033[1A\033[20C ok\nRunning your code...")
    vm.run()

def catch_ctrl_c(sig, frame):
    print("\rExecution interrupted.")
    exit(1)

if __name__ == "__main__":
    signal.signal(signal.SIGINT, catch_ctrl_c)
    main()
