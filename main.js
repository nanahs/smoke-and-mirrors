import "./style.css"
import { Elm } from "./src/Main.elm"
import "elm-canvas"

const node = document.querySelector("main")
const app = Elm.Main.init({node: node});
