## Erlang Pipes

#### Overview

This is a utility application used to communicate from Erlang application with the shell process via standard input and outout. The application could be used to pass processing of some data from Erlang (as a Erlang 'iolist', 'binary' or 'string') to some external shell script. The script need to be executable and able to take input from the standard input and send the resulting output to the standard output.

The application spawns a separate process for execution of the shell script, which dies when script sends back EOF or ends its execution. There is no upper limit on the number of spawned processes.

#### Quick start:

To build and run application, you need to have Erlang rebar and make tools installed.

Quick run guide:

        $ make
        $ make run

and then you can invoke sample shell script:

    ...
    > epipes:exec("tolower.awk", "ASADSasda").
    {ok,[<<"asadsasda\n">>]}
    > epipes:exec("/usr/bin/sed s/day/night/g", <<"Good day!">>).
    {ok,[<<"Good night!\n">>]}
    ...

#### Credits:

- to [Matt Stancliff] (https://github.com/mattsta) for stdin/stdout proxy - 'stdin_forcer'

[![endorse](http://api.coderwall.com/systra/endorse.png)](http://coderwall.com/systra)
