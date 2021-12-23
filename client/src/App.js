import React, { useState, useRef, useEffect } from 'react'
import Board from './Board'
import './App.css'

const App = () => {
  const [wsMessage, setWsMessage] = useState('')
  const [sessionId, setSessionId] = useState(null)
  const webSocket = useRef(null)
  const [field, setField] = useState('')
  const [pieces, setPieces] = useState(Array(19 * 19).fill('n', 0, 19 * 19))
  const [color, setColor] = useState('')
  const [victor, setVictor] = useState('')
  const [isAiGame, setIsAiGame] = useState(false)

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
      webSocket.current = new WebSocket("ws://" + window.location.hostname + ":8080/game/connect");
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
        .then(data => setVictor(data['message']))
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

  const handleStartAiGame = () => {
    fetch("/game/create_ai_opponent", {
      method: 'POST',
      credentials: 'include',
    }).then(() => setIsAiGame(true))
  }

  function handleSquareClick(i) {
    let turn = wsMessage
    if (wsMessage === 'game_start') {
      turn = 'b'
    }
    if (turn === color && pieces[i] === 'n') {
      let url = ''
      if (isAiGame) {
        url = '/game/make_move_ai'
      } else {
        url = '/game/make_move'
      }
      fetch(url, {
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
      <div id="bg">
        <div class="flex-container-1">
          <div class="gameinfo">
            <div class="whitepanel">
              <header>You are playing<br/><b>{color === 'b' ? "black" : "white"}</b> pieces.</header>
            </div>
            <div class="whitepanel">
              <header>{turn === "game_complete" ?
                ("Game over: ".concat(victor === 'b' ? "black" : "white").concat(" wins.")) :
                ("It is ".concat(turn === 'b' ? "black's" : "white's").concat(" turn."))}
              </header>
            </div>
          </div>
          <div class="mainbox">
            <Board pieces={pieces} onClick={i => handleSquareClick(i)} />
          </div>
        </div>
      </div>
    )
  } else {
    return (
      <div id = "bg">
        <div class = "flex-container-2">
        <p id="title">五caml</p>
        <br/>Your assigned session ID is:
        <form>
          <label>
            <input type="text" name="assignedID" value={sessionId}/>
          </label>
        </form>
        <br/>Want to join someone else's game? Type their ID into here:
        <form onSubmit={handleSubmit}>
          <label>
            <input type="text" name="name" onChange={handleInputChange} />
          </label>
          <input type="submit" value="Submit" />
        </form>
        <br/>Want to play an AI game?
        <button onClick={handleStartAiGame}>Start AI game</button>
        </div>
      </div>
    )
  }
}

export default App;
