package de.htwg.se.mill.aview

import scala.io.StdIn.readLine
import de.htwg.se.mill.util.Observer
import de.htwg.se.mill.controller.Controller
import de.htwg.se.mill.util.Messages

final case class TUI(controller: Controller) extends Observer {
  controller.add(this)
  def run = {
    println("""
Welcome to Muehle a strategy board game.
To set or remove a piece please use a command like 123
where 1 stands for the first column, 2 stands for the second row
and 3 stands for the third ring.
To move a piece please use a command like 111 112 where the first
part of the command 111 indicates the piece field before moving
and the part of the command 112 indicates the piece field after moving.
You can exit the game by pressing q key or start a new game by pressing n key.

Before starting please enter the name of the first player.""")
    controller.addFirstPlayer(readLine)
    println(
      "Now please enter the name of the second player to play."
    )
    controller.addSecondPlayer(readLine)
    controller.newGame
    inputLoop
  }
  override def update(message: Option[String]) = {
    println(
      if (message.isDefined) message.get
      else controller.gameState.get.game.board
    )
  }
  private def inputLoop: Unit = {
    val input = readLine(
      s"${controller.gameState.get.game.currentPlayer}'s turn(${controller.currentGameState}): "
    )
    if (onInput(input)) return
    inputLoop
  }

  def onInput(
      input: String
  ): Boolean = {
    val currentGameState = controller.gameState.get
    val currentBoard = currentGameState.game.board

    input match {
      // quit the game
      case "q" => true
      // start a new game
      case "n" => {
        controller.newGame
        return false
      }
      // undo a turn
      case "u" => {
        controller.undo
        return false
      }
      // redo a turn
      case "r" => {
        controller.redo
        return false
      }
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
          update(Some(Messages.wrongSettingOrRemovingCommandMessage))
          return false
        }
        if (
          (controller.isMovingOrFlying) && !input
            .matches(s"$commandPattern $commandPattern")
        ) {
          update(Some(Messages.wrongMovingOrFlyingCommandMessage))
          return false
        }

        val fields = input.split(" ").map(field => field.split(""))
        val field = currentBoard.getField(
          fields(0)(0).toInt - 1,
          fields(0)(1).toInt - 1,
          fields(0)(2).toInt - 1
        )
        if (field.isEmpty) {
          update(Some(Messages.wrongFieldPositionMessage))
          return false
        }
        if (fields.length > 1) {
          val to = currentBoard.getField(
            fields(1)(0).toInt - 1,
            fields(1)(1).toInt - 1,
            fields(1)(2).toInt - 1
          )
          if (to.isEmpty) {
            update(Some(Messages.wrongTargetFieldPositionMessage))
            return false
          }
          if (controller.movePiece(field.get, to.get).isDefined) {
            return false
          }
        } else {
          if (
            controller.isSetting && controller.setPiece(field.get).isDefined
          ) {
            return false
          }
          if (
            controller.isRemoving && controller
              .removePiece(field.get)
              .isDefined
          ) {
            return false
          }
        }
        return false
      }
    }
  }
}
