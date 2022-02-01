process.stdout.isTTY = true
import compiler from 'node-elm-compiler'
import fs from 'fs'
import process from 'process'
import {__dirname, __node_modules} from './__dirname.mjs'
import {join} from "path"


export const getReport= async path => {
    const elmJsonPath = process.cwd() + "/elm.json"
    const elmJson = JSON.parse( (await fs.promises.readFile(elmJsonPath)).toString())
    const watchDeps =
        [ elmJsonPath
        , ...elmJson['source-directories'].map(srcDir => join(process.cwd(), srcDir, "**", "*.elm"))
        ]

    const elmFile = process.env.ELM_MAIN || 'src/Main.elm'
    const dependencies = await compiler.findAllDependencies(elmFile);
    try {
        const compiledApp =
            await compiler.compileToString(
                [elmFile]
                    , { output: '.js', pathToElm: __node_modules + '/elm/bin/elm'
                    , report : 'json'
                    });
        return ["COMPILED", watchDeps]
    } catch (e) {
        try  {
        const report = JSON.parse(e.message.split('\n')[1])
        report.errors = report.errors || []


        await Promise.all(report.errors.map(async compileError => {
            const contents = (await fs.promises.readFile(compileError.path)).toString()
            compileError.code = contents
            return {}
        }))
        return [report, watchDeps]
        } catch (postProcessError)  {
            console.log(postProcessError)
            console.log('ERROR', e.message)
        }
    }
}
