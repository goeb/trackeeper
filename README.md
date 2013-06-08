
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
- install procedure on Linux
- web interface for :
    creating new project
    modifying columns of project
    templating project (apply a column structure to a project)
- filter by field (simple) -> done
- user authentication
- user's guide
- developer's guide
- unit tests

v0.2
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

METHOD + scheme://server/resource/action + parameters
scheme: http or https
resource:
    /
    /project
    /project/issue
    /project/issue/N

action:
    /login
    /new
    /list



