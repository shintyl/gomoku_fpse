switch ReactDOM.querySelector("#root") {
| Some(root) => ReactDOM.render(<div> {React.string("Hello world!")} </div>, root)
| None => () // do nothing
}
