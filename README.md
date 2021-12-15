Prerequisites
 - Node.js and the npm command line interface (npm version 8.1.4 and node version 16.13.0 tested and working, but other versions will also likely work)
 - OCaml 4.12.0

Package installation
 - From the ```client``` directory, run ```npm install```
 - From the ```api``` directory, run ```opam install```

Building and running
 - You will need at least 2 terminal windows open (or you can background one of the processes)
 - In one terminal window, run ```npm start``` from the ```client``` directory. This will first build the React client and start a development server that will serve the client code.
 - In another terminal window, run ```dune exec src/server.exe``` from the ```api``` directory. This will first build server.exe and then run it.

Accessing the application
 - Navigate to ```localhost:3000``` in a web browser
