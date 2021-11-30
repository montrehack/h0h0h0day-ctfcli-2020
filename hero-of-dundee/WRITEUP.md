## Flag 1
### How to
Connect using ssh with the given credentials. Once on the machine, 

```sh
sudo -l
```
This will show that you're able to run vim as another user. 

```sh
sudo -u
```

### Flag
FLAG-{Th@t$AG0od$t@rt}

---
## Flag 2
### How to
In index.php on the webserver port

### Flag
FLAG-{83tt3rLo0k@tTh3$0urc3$}

---
## Flag 3
### How to
In robots.txt on the webserver port

### Flag
FLAG-{MrR0b0t$}

---
## Flag 4 - Optionnal
### How to
Enable the debug cookie. Note this one will help a lot for the 5th flag

### Flag
FLAG-{Co0ki3$Ftw}

---
## Flag 5
### How to
You need to be able to retrieve the db's table name aswell as its columns, in order to be able to perform a UNION ALL injection.

This way, you'll be able to get admin's password (along as its username). 

Then, you just need to netcat onto the openned tcp port on the host machine, and login with the admin's password. However, there's a catch on this one, as the password was not stored in clear ofc... (ROT13 encoded)

### Sql Injection patterns
``` sql
%" --
" UNION SELECT name FROM sqlite_master WHERE type='table' --
" UNION SELECT sql FROM sqlite_master WHERE name='users' --
%" UNION ALL SELECT mysecretpwd FROM users --
```

### Flag
FLAG-{Th@t$Th3Fin@lF|@g}
