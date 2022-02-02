#! node
import http from "http"


import ws from 'ws'
import * as Y from "yjs"
import {getReport} from "./compiler.mjs"
import fs from "fs"
import chokidar from "chokidar"
import {__node_modules} from "./__dirname.mjs"

const viewState = new Y.Doc()


const server = http.createServer((request, res) => {
    res.setHeader("Access-Control-Allow-Origin", "*");
    res.setHeader("Access-Control-Allow-Headers", "*");

    switch(request.method) {
        case "GET":
            refreshReport(request, res)
            break;
        case "POST":
            console.log("APPLYING FIX")
            applyFix(request, res)
            break;
        case "OPTIONS":
            res.write("OK")
            res.end()
            break;
    }

})



const refreshReport = async (request, res) => {
    console.log("COMPILING", new Date())
    try {
        const [report, deps] = await getReport()
        watcher.add(deps)
        viewState.getMap().set('report', report)
        console.log("COMPILED", new Date())
        res && res.end()
    } catch (e) {
        console.log("COMPILATION FAILED", new Date())
    }
}

const watcher = chokidar.watch()
watcher.on('change', ch => {
    console.log(ch)
    refreshReport()
})

const applyFix = async (request, res) => {
    var body = "";
    request.on('data', function(chunk) {
        body += chunk;
    });
    request.on('end', async function() {

        const {path, code} = JSON.parse(body)
        console.log("WRITE", path)
        await fs.promises.writeFile(path, code)
        res.write("OK");
        await refreshReport(request, res)
        res.end()
    });
}
main()
async function main() {

    const {WebsocketProvider} = await import(__node_modules + '/y-websocket/dist/y-websocket.cjs')

    const wsProvider = new WebsocketProvider('ws://localhost:4321', 'elm-interreactor', viewState, { WebSocketPolyfill: ws })

    var connected = false

    wsProvider.on('status', event => {
      console.log('Websocket to compiler server', event.status) // logs "connected" or "disconnected"
    })

    server.listen(8787)
}
