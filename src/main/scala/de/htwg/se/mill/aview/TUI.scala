package de.htwg.se.mill.aview

import scala.io.StdIn.readLine
import de.htwg.se.mill.util.Observer
import de.htwg.se.mill.util.Messages
import de.htwg.se.mill.util.Event
import scalafx.application.Platform
import de.htwg.se.mill.controller.ControllerInterface

class TUI(val controller: ControllerInterface) extends Observer {
  controller.add(this)
  var quit = false
  def start = {
    println(Messages.introductionText)
    controller.addFirstPlayer(readLine)
    println(
      Messages.addSecondPlayerText
    )
    controller.addSecondPlayer(readLine)
    controller.newGame
  }
  override def update(message: Option[String], e: Event) = {
    e match {
      case Event.QUIT => quit = true
      case Event.PLAY =>
        println(
          if (message.isDefined) message.get
          else
            s"${controller.gameState.get.game.currentPlayer}'s turn(${controller.currentGameState}): "
              + controller.gameState.get.game.board
        )
    }
  }
  def run: Unit = {
    val input = readLine()
    Platform.runLater(onInput(input))
    if (quit) return
    run
  }

  def onInput(
      input: String
  ): Unit = {
    val currentGameState = controller.gameState.get
    val currentBoard = currentGameState.game.board

    input match {
      // quit the game
      case "q" => controller.quit
      // start a new game
      case "n" => controller.newGame
      // undo a turn
      case "u" => controller.undo
      // redo a turn

      case "r" => controller.redo
      // save the current game state
      case "s" => controller.save
      // load the most recent game state
      case "l" => controller.load

      /*
        play the game
        input notation: (columnrowring) e.g. 111 121 or 111
       */
      case _ => {
        val commandPattern = s"[1-${currentBoard.size}]{3}"

        if (
          (controller.isSetting || controller.isRemoving) && !input
            .matches(commandPattern)
        ) {
          update(
            Some(Messages.wrongSettingOrRemovingCommandMessage),
            Event.PLAY
          )
          return
        }
        if (
          (controller.isMovingOrFlying) && !input
            .matches(s"$commandPattern $commandPattern")
        ) {
          update(Some(Messages.wrongMovingOrFlyingCommandMessage), Event.PLAY)

          return
        }

        val fields = input.split(" ").map(field => field.split(""))
        val field = currentBoard.getField(
          fields(0)(0).toInt - 1,
          fields(0)(1).toInt - 1,
          fields(0)(2).toInt - 1
        )
        if (field.isEmpty) {
          update(Some(Messages.wrongFieldPositionMessage), Event.PLAY)
          return
        }
        if (fields.length > 1) {
          val to = currentBoard.getField(
            fields(1)(0).toInt - 1,
            fields(1)(1).toInt - 1,
            fields(1)(2).toInt - 1
          )
          if (to.isEmpty) {
            update(Some(Messages.wrongTargetFieldPositionMessage), Event.PLAY)

            return
          }
          if (controller.movePiece(field.get, to.get).isDefined) return
        } else {
          if (controller.isSetting && controller.setPiece(field.get).isDefined)
            return
          if (
            controller.isRemoving && controller
              .removePiece(field.get)
              .isDefined
          ) return
        }
      }
    }
  }
}
