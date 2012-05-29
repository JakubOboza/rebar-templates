# Instalation

First clone the repo `git clone https://github.com/JakubOboza/rebar-templates` next symlink to `~/.rebar/templates` eg.
`ln -s /Users/kuba/Workspace/Erlang/rebar-templates /Users/kuba/.rebar/templates` and have fun!

# Usage

Currently i have two templates webmachine resource and gen_server. To generate webmachine resource do this:

    λ rebar create template=webmachine_resource name=simple app_name=my description="Cool resource"

this will generate file:

    ==> src (create)
    Writing src/my_simple_resource.erl

:)

## Templates

* template=webmachine_resource options[name, app_name, copyright_year, author_name, author_email] 
* template=gen_server  options[name, author_name, author_email, copyright_year]

# Author
Jakub Oboza: <http://no-fucking-idea.com>§