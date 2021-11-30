# Ho Ho No - Part 1 - Writeup by Zuyoutoki
## The challenge
We need to recover a file whose named `1337` from an unfamiliar computer, running an unfamiliar operating system called "Ho Ho Hoperating System". We get an SSH access to the system and a PDF serving as a user manual. 
## The SSH prompt
```
.-----------------------------------------------------.
| Ho Ho Hoperating System                     v0.2020 |
'-----------------------------------------------------'
.-----------------------------------------------------.
| Welcome to the H3S!                                 |
| - - - - - - - - - - - - - - - - - - - - - - - - - - |
| Please make sure that you have read the end user    |
| manual which came with the computer on the official |
| floppy disk before trying to contact support.       |
| - - - - - - - - - - - - - - - - - - - - - - - - - - |
| All the instructions that you might need are        |
| documented and we even included one code sample.    |
'-----------------------------------------------------'
> Enter your code. An empty line will execute

```
It asks for some code, and runs it when an empty line is sent. We are given code execution on the machine, but we need to write our payload using the H3S language defined in the End User Manual.
## The solve
We can use the following code to get the content of the file `1337`:
```
SET G01 1337
LOD G01
MTR G01 G02
CMP G01 G03
JEQ FFFF
INC G02
ACB G01
PRT
JMP 0002
```

And we get the following output:
```
Parsing your code... ok
Running your code...
You expected a nice list, but it was me DIO!

I felt a little mischievious so I replaced the nice list with my gift suggestions for this year!
Please take those in consideration when the time comes:
  - reusable metal straw (for the planet)
  - Art of War, by Sun Tsu (for the culture)
  - icepack (for keeping my vaccine cool)
  - cookies and milk (definitely not for overthrowing Santa next year, hehe)
  - a flag (the kind with FLAG-LetsLearnANewLanguageForTheWinterSeason written on the back)

Execution completed in 3491 cycle(s)
```

The flag is : FLAG-LetsLearnANewLanguageForTheWinterSeason
