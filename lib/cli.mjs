#!/usr/bin/env node

import {__dirname} from './__dirname.mjs'
import execa from "execa"
import {Writable} from "stream"
import {__node_modules} from "./__dirname.mjs"

class HandleServer extends Writable {
  constructor(options) {
    super(options);
    this.name = options
  }
  _write(chunk, encoding, callback) {
    if (this.name === 'websocket' && chunk.toString().indexOf(4321) > -1) {
        startCompilerServer()
    }
    if (this.name === 'ui') {
        console.log(chunk.toString())
    }
  }
}

const startCompilerServer = async () => {
    try {
        await import(__dirname + '/server.mjs')
    } catch (e) {
        console.warn(e)
        process.exit(1)
    }
}

main()
async function main () {
    execa(__node_modules + "/http-server/bin/http-server", [__dirname + "/../dist"]).stdout.pipe( new HandleServer('ui') )
    execa(__node_modules + "/y-websocket/bin/server.js", [], {env: {PORT: 4321}}).stdout.pipe( new HandleServer('websocket') )
}
