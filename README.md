# trafini - Task Management Tool with http interface
## What is this?
A simple task management server which can be accessed through http interface.

## How to use?
Simply, run trafini server,
```
$ trafini data.json
```
then you can access it through http interface on port 3000.

### Example
```
POST http://localhost:3000/
param: {q: <your-json-query>}
```
q is query which is jsonized array object below.

+ Show all tasks
    - ["show"]
+ Show tasks with specific tags
    - ["show", "your,tags,here"]
+ Show details of task (id = 'abcde')
    - ["detail", "abcde"] (normal)
    - ["detail", "abc"] (if unique, this is ok)
    - ["detail", "a"] (if unique, this is ok)
+ Set task (id = 'abcde')
    - ["set", "abcde", "priority=4", "tags=aaa,bbb,ccc", "summary=hogehoge", "detail=hugahuga"]
        * you can abbreviate priority to "p", tags to "t"...
+ Add task
    - ["add", "priority=4", "tags=aaa", "summary=dowork"]
        * property is the same to set task
+ Finish task (id = 'abcde')
    - ["finish", "abcde"]
+ Unfinish task (id = 'abcde')
    - ["unfinish", "abcde"]
