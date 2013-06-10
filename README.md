
Trackeeper (TKE) - Issue Tracking System
----------------------------------------

Getting Started
---------------
TODO

HTTP Interface
--------------
TODO


Roadmap
-------

v0.1
- create, edit issues, comments -> done
- full-text search              -> done
- power up multi projects       -> done
- filter by field (simple)      -> done
- install procedure on Linux
- create new project (web ui)
- modify columns of existing project (web ui)
- user authentication           -> done
- access rights
- user's guide
- developer's guide
- unit tests

v0.2
- modify display of columns (order, and how many on the same row) (web ui)
- ability to edit message until a subsequent other message
- templating project (apply a column structure to a project)
- file upload
- functional tests (curl + format=erlang)
- full-contents view (eveything on a single HTML page, for PDF export, etc.)
- configuration of web interface (menus, pre-defined searchs)

v0.3
- enable offline read-only copy of the TKE base
- unicode
- install procedure on Windows

v0.4
- enable offline read-write copy of the TKE base
- merge tool (for merging into the main TKE base)


future ?
- rich text


REST API

METHOD + scheme://server/resource + parameters
scheme: http or https
resource:
    /
        global resource, above projects, used to create new project
        POST: create new project
        GET: print list of allowed project

    /project
        project level resource (not used at the moment)
        POST: undefined
        GET: undefined

    /project/issue
        resource global to all issues
        POST: undefined
        GET: print list of all issues of the project

    /project/issue/new
        GET: print form for creating new issue
        POST: create new issue

    /project/issue/N
        a single issue
        GET: print issue contents
        POST: update issue N

    /login
        login related resource
        GET: print form for login
        POST: submit identification


old actions:
    /login
    /new
    /list



