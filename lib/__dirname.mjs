// get dirname
import { dirname, normalize, join } from 'path';
import { fileURLToPath } from 'url';
import fs from 'fs'
export const __dirname = dirname(fileURLToPath(import.meta.url));
console.log(__dirname)

const findNodeModules = (dirname) => {
    const assumed_node_modules = join(dirname, 'node_modules')

    if (fs.existsSync(assumed_node_modules)) {
        // Do something
        return normalize(assumed_node_modules)
    } else {
        // go one up
        return findNodeModules(join(dirname, '..'))
    }
}
export const __node_modules = findNodeModules(__dirname)
console.log('node', __node_modules)
