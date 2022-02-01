import elm from "vite-plugin-elm"

// we go through index.html which import src/client.js
export default {
    plugins: [
        elm({debug: true})
    ]
    , server : {open: true }
}
