import './Board.css'

const Square = (props) => {
    let v = props.value === 'n' ? '' : props.value

    if (v === 'b') {
        v = '●'
    } else if (v === 'w') {
        v = '○'
    }

    return (
        <button className="square" onClick={props.onClick}>
            {v}
        </button>
    )
}

const Board = (props) => {
    const renderSquare = (i) => {
        return (
            <Square
                key={i}
                value={props.pieces[i]}
                onClick={() => props.onClick(i)}
            />
        )
    }

    const renderBoard = () => {
        let rows = []
        let row_num = 0
        let k = 0
        for (let i = 0; i < 19; i++) {
            let cur = []
            for (let j = 0; j < 19; j++) {
                cur.push(renderSquare(k))
                k++
            }
            rows.push(<div key={'r'.concat(row_num.toString())} className="board-row">{cur}</div>)
            row_num++
        }
        return rows
    }

    return (
        <div>
            {renderBoard()}
        </div>
    )

}

export default Board