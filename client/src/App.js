import React, { useState, useRef, useEffect } from 'react'
import Board from './Board'

const App = () => {
  const [wsMessage, setWsMessage] = useState('')
  const [sessionId, setSessionId] = useState(null)
  const webSocket = useRef(null)
  const [field, setField] = useState('')
  const [pieces, setPieces] = useState(Array(19 * 19).fill('n', 0, 19 * 19))
  const [color, setColor] = useState('')

  useEffect(() => {
    fetch("/session/refresh", {
      method: 'POST',
      credentials: 'include'
    })
      .then(response => response.json())
      .then(data => setSessionId(data['message']))
  }, [])

  useEffect(() => {
    if (sessionId) {
      webSocket.current = new WebSocket("ws://localhost:8080/game/connect");
      webSocket.current.onmessage = (message) => {
        setWsMessage(message.data)
      }
      return () => webSocket.current.close();
    }
  }, [sessionId])

  useEffect(() => {
    if (wsMessage === 'game_start') {
      fetch("/game/color", {
        method: 'GET',
        credentials: 'include',
      })
        .then(response => response.json())
        .then(data => setColor(data['message']))
    } else if (wsMessage === 'game_complete') {
      updatePieces();
      fetch("/game/winner", {
        method: 'GET',
        credentials: 'include',
      })
        .then(response => response.json())
        .then(data => alert('winner is '.concat(data['message'])))
    } else if (wsMessage) {
      updatePieces()
    }
  }, [wsMessage])

  const updatePieces = () => {
    fetch("/game/board", {
      method: 'GET',
      credentials: 'include'
    })
      .then(response => response.json())
      .then(data => {
        let new_pieces = Array(19 * 19).fill('n', 0, 19 * 19)
        data.forEach(elt => {
          let idx = (elt['x'] * 19) + elt['y']
          new_pieces[idx] = elt['c']
        })
        setPieces(new_pieces)
      })
  }

  const handleInputChange = (e) => {
    setField(e.target.value)
  }

  const handleSubmit = (e) => {
    e.preventDefault()
    fetch("/game/create", {
      method: 'POST',
      credentials: 'include',
      body: JSON.stringify({ message: field })
    })
      .then(response => {
        if (response.status !== 200) {
          alert("Could not find other session")
        }
      })
  }

  function handleSquareClick(i) {
    let turn = wsMessage
    if (wsMessage === 'game_start') {
      turn = 'b'
    }
    if (turn === color && pieces[i] === 'n') {
      fetch("/game/make_move", {
        method: 'POST',
        credentials: 'include',
        body: JSON.stringify({
          x: Math.floor(i / 19),
          y: i % 19
        })
      })
    }
  }

  if (wsMessage) {
    let turn = wsMessage
    if (wsMessage === 'game_start') {
      turn = 'b'
    }
    return (
      <div>
        <header>Your color: {color}</header>
        <header>Turn: {turn}</header>
        <Board pieces={pieces} onClick={i => handleSquareClick(i)} />
      </div>
    )
  } else {
    return (
      <div>
        {sessionId}
        <form onSubmit={handleSubmit}>
          <label>
            Session ID:
            <input type="text" name="name" onChange={handleInputChange} />
          </label>
          <input type="submit" value="Submit" />
        </form>
      </div>
    )
  }

}

export default App;
