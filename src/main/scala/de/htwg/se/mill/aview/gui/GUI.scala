package de.htwg.se.mill.aview

import de.htwg.se.mill.util.Observer
import de.htwg.se.mill.util.Event
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import javafx.scene.paint.Color
import scalafx.Includes.*
import scalafx.scene.layout.StackPane
import de.htwg.se.mill.aview.gui.Ring
import scalafx.application.Platform
import scalafx.scene.layout.Background
import scalafx.scene.layout.BackgroundImage
import scalafx.scene.image.Image
import scalafx.scene.layout.BackgroundRepeat
import scalafx.scene.layout.BackgroundPosition
import scalafx.geometry.Side
import scalafx.scene.layout.BackgroundSize
import de.htwg.se.mill.model.FieldInterface
import de.htwg.se.mill.aview.gui.Board
import de.htwg.se.mill.aview.gui.MessageBox
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.text.Font
import scalafx.scene.control.TextInputDialog
import de.htwg.se.mill.util.Messages
import de.htwg.se.mill.controller.ControllerInterface

class GUI(val controller: ControllerInterface) extends JFXApp3 with Observer {
  controller.add(this)

  def onAction: (field: FieldInterface) => Unit = (field: FieldInterface) => {
    if (controller.isSetting) {
      controller.setPiece(field)
    } else if (controller.isRemoving) {
      controller.removePiece(field)
    } else {
      if (controller.fromField.isDefined) {
        controller.movePiece(controller.fromField.get, field)
        controller.fromField = None
      } else {
        controller.fromField = Some(field)
      }
    }
  }
  override def update(message: Option[String], e: Event): Unit = {
    if (message.isDefined) {
      Platform.runLater {
        new Alert(AlertType.Warning) {
          initOwner(stage)
          headerText = message.get
        }.showAndWait()
      }
    }
    e match
      case Event.QUIT => Platform.exit()
      case Event.PLAY => start()
  }

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "Mill"
      scene = new Scene(800, 600) {
        resizable = false
        root = {
          if (controller.gameState.isEmpty) {
            val firstPlayerName = new TextInputDialog() {
              initOwner(stage)
              title = "Add first Player"
              headerText = Messages.introductionText
              contentText = "Please enter the name of the first player:"
            }.showAndWait()

            firstPlayerName match {
              case Some(name) => controller.addFirstPlayer(name)
              case None => {
                println("Add first Player Dialog was canceled.")
                sys.exit()
              }
            }
            val secondPlayerName = new TextInputDialog() {
              initOwner(stage)
              title = "Add second Player"
              headerText = Messages.addSecondPlayerText
              contentText = "Please enter the name of the second player:"
            }.showAndWait()

            secondPlayerName match {
              case Some(name) => controller.addSecondPlayer(name)
              case None => {
                println("Add second Player Dialog was canceled.")
                sys.exit()
              }
            }
            controller.newGame
          }
          Board(controller, onAction)
        }
      }
    }
  }
}
