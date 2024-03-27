package de.htwg.se.mill.controller

import scalafx.application.Platform
import de.htwg.se.mill.model.Game
import de.htwg.se.mill.model.Board
import de.htwg.se.mill.model.Player
import scala.util.{Try, Success, Failure}
import de.htwg.se.mill.model.WinStrategy
import de.htwg.se.mill.model.GameState
import de.htwg.se.mill.model.GameEvent
import de.htwg.se.mill.model.{
  SettingState,
  RemovingState,
  MovingState,
  FlyingState
}
import de.htwg.se.mill.util.Event
import de.htwg.se.mill.model.FieldInterface
import de.htwg.se.mill.model.BoardInterface
import de.htwg.se.mill.model.PlayerInterface
import de.htwg.se.mill.model.GameInterface
import com.google.inject.Inject
import com.google.inject.Guice
import de.htwg.se.mill.MillModule
import de.htwg.se.mill.model.FileIOInterface
import de.htwg.se.mill.util.Messages

class Controller @Inject() (private val board: BoardInterface)
    extends ControllerInterface {
  private val twoPlayers = new Array[PlayerInterface](2)
  private val winStrategy = WinStrategy.classicStrategy
  private var previousTurn: Option[Try[GameState]] = None
  private var undoCommand = new UndoCommand()
  var gameState: Option[GameState] = None
  var fromField: Option[FieldInterface] = None

  def undo: Option[Throwable] = undoCommand.undoStep
  def redo: Option[Throwable] = undoCommand.redoStep

  def addFirstPlayer(playerName: String, playerColor: String = "🔴") = {
    twoPlayers(0) = Player(playerName, playerColor)
  }
  def addSecondPlayer(playerName: String, playerColor: String = "🔵") = {
    twoPlayers(1) = Player(playerName, playerColor)
  }
  def newGame = {
    // delete command history
    undoCommand = new UndoCommand()
    gameState = Some(
      SettingState(
        Game(
          board,
          twoPlayers,
          twoPlayers(0)
        )
      )
    )
    previousTurn = Some(Success(gameState.get))
    notifyObservers(None, Event.PLAY)
  }

  def quit = notifyObservers(None, Event.QUIT)

  private def createSnapshot: Snapshot = {
    val snapshot = new Snapshot(this, previousTurn)
    return snapshot
  }

  // Memento
  private class Snapshot(
      val controller: Controller,
      val previousTurn: Option[Try[GameState]]
  ) {
    def restore: Option[Throwable] =
      controller.previousTurn = previousTurn
      previousTurn.get match {
        case Success(state: GameState) => {
          state match {
            case RemovingState(game: GameInterface) => {
              controller.gameState = Some(state)
            }
            case SettingState(game: GameInterface) => {
              controller.gameState = Some(state)
            }
            case FlyingState(game: GameInterface) => {
              controller.gameState = Some(state)
            }
            case MovingState(game: GameInterface) => {
              controller.gameState = Some(state)
            }
          }
          controller.notifyObservers(None, Event.PLAY)

          return None
        }
        case Failure(error) => {
          controller.notifyObservers(Some(error.getMessage()), Event.PLAY)

          return Some(error)
        }
      }
  }

  // Command
  private class UndoCommand {
    private var undoStack: List[Snapshot] = Nil
    private var redoStack: List[Snapshot] = Nil
    def backup(snapshot: Snapshot): Unit = {
      undoStack = snapshot :: undoStack
    }
    def undoStep: Option[Throwable] =
      undoStack match {
        case Nil => None
        case head :: stack => {
          val result = head.restore
          undoStack = stack
          redoStack = head :: redoStack
          result
        }
      }
    def redoStep: Option[Throwable] =
      redoStack match {
        case Nil => None
        case head :: stack => {
          val result = head.restore
          redoStack = stack
          undoStack = head :: undoStack
          result
        }
      }
  }

  def setPiece(to: FieldInterface): Option[Throwable] = {
    undoCommand.backup(createSnapshot)
    doTurn(
      gameState.get.handle(
        GameEvent.OnSetting,
        (to, None)
      )
    )
  }

  def movePiece(from: FieldInterface, to: FieldInterface): Option[Throwable] = {
    undoCommand.backup(createSnapshot)
    doTurn(
      gameState.get.handle(
        GameEvent.OnMoving,
        (from, Some(to))
      )
    )
  }

  def removePiece(field: FieldInterface): Option[Throwable] = {
    undoCommand.backup(createSnapshot)
    doTurn(
      gameState.get.handle(
        GameEvent.OnRemoving,
        (field, None)
      )
    )
  }
  def currentGameState = gameState.get match {
    case FlyingState(game: GameInterface)   => "Flying Pieces"
    case MovingState(game: GameInterface)   => "Moving Pieces"
    case SettingState(game: GameInterface)  => "Setting Pieces"
    case RemovingState(game: GameInterface) => "Removing Pieces"
  }

  def isSetting = gameState.get.isInstanceOf[SettingState]
  def isRemoving = gameState.get.isInstanceOf[RemovingState]

  def isMovingOrFlying =
    gameState.get.isInstanceOf[MovingState] || gameState.get
      .isInstanceOf[FlyingState]

  def save: Unit = {
    try {
      Guice
        .createInjector(new MillModule)
        .getInstance(classOf[FileIOInterface])
        .save(previousTurn.get.get)
      notifyObservers(
        None,
        Event.PLAY
      )
    } catch {
      case e: Exception =>
        notifyObservers(Some(Messages.gameStateCouldNotBeSaved), Event.PLAY)
    }
  }

  def load: Unit = {
    try {
      doTurn(
        Success(
          Guice
            .createInjector(new MillModule)
            .getInstance(classOf[FileIOInterface])
            .load
        )
      )
    } catch {
      case e: Exception =>
        notifyObservers(Some(Messages.gameStateCouldNotBeLoaded), Event.PLAY)
    }
  }

  private def doTurn(turn: Try[GameState]): Option[Throwable] = {
    previousTurn = Some(turn)
    turn match {
      case Success(state: GameState) => {
        var currentGame: Option[GameInterface] = None
        state match {
          case RemovingState(game: GameInterface) => {
            gameState = Some(state)
            currentGame = Some(game)
          }
          case SettingState(game: GameInterface) => {
            gameState = Some(
              SettingState(
                game.copyCurrentPlayer(currentPlayer =
                  twoPlayers.find(p => !p.equals(game.currentPlayer)).get
                )
              )
            )
            currentGame = Some(game)
          }
          case FlyingState(game: GameInterface) => {
            gameState = Some(
              FlyingState(
                game.copyCurrentPlayer(currentPlayer =
                  twoPlayers.find(p => !p.equals(game.currentPlayer)).get
                )
              )
            )
            currentGame = Some(game)
          }
          case MovingState(game: GameInterface) => {
            gameState = Some(
              MovingState(
                game.copyCurrentPlayer(currentPlayer =
                  twoPlayers.find(p => !p.equals(game.currentPlayer)).get
                )
              )
            )
            currentGame = Some(game)
          }
        }

        if (winStrategy(currentGame.get)) {
          notifyObservers(
            Some(
              s"Congratulations! ${currentGame.get.currentPlayer} has won the game!\nStarting new game."
            ),
            Event.PLAY
          )

          newGame
        } else {
          notifyObservers(None, Event.PLAY)

        }
        return None
      }
      case Failure(error) => {
        notifyObservers(Some(error.getMessage()), Event.PLAY)

        return Some(error)
      }
    }
  }
}
