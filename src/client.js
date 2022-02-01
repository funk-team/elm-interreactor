import { Elm } from './Client.elm';
import * as Y from "yjs"
import { WebsocketProvider } from 'y-websocket'

const viewState = new Y.Doc()

const wsProvider = new WebsocketProvider('ws://localhost:4321', 'elm-interreactor', viewState)

wsProvider.on('sync', is => {
    const node = document.querySelector("#app")
    if (!is || !node) {
        return
    }
    const client = Elm.Client.init({node})
    client.ports.gotReport.send(viewState.getMap().toJSON())
    viewState.on('update', update => {
        console.log(viewState.getMap().toJSON())
        client.ports.gotReport.send(viewState.getMap().toJSON())
    })
})

