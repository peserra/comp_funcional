{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import Data.Int (Int32)
--import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)

-- Função para abrir um FileChooserDialog para salvar um arquivo
openSaveDialog :: IO (Maybe FilePath)
openSaveDialog = do
    dialog <- new Gtk.FileChooserDialog [ #action := Gtk.FileChooserActionSave
                                        , #title := "Save File"
                                        ]
    _ <- Gtk.dialogAddButton dialog "Cancel" (fromIntegral (fromEnum Gtk.ResponseTypeCancel))
    _ <- Gtk.dialogAddButton dialog "Save" (fromIntegral (fromEnum Gtk.ResponseTypeAccept))
    Gtk.fileChooserSetDoOverwriteConfirmation dialog True

    response <- Gtk.dialogRun dialog
    if response == fromIntegral (fromEnum Gtk.ResponseTypeAccept)
        then do
            -- Obtém o caminho do arquivo selecionado
            maybeFilePath <- Gtk.fileChooserGetFilename (Gtk.castTo Gtk.FileChooser dialog)
            -- Fecha o diálogo após o uso
            Gtk.widgetDestroy dialog
            return maybeFilePath
        else do
            -- Fecha o diálogo se o usuário cancelar
            Gtk.widgetDestroy dialog
            return Nothing
-- chat gpt com modificações

highlightWord word buffer = do
      -- Cria uma tag para aplicar uma cor (ex: vermelho)
    tagRed <- new Gtk.TextTag [ #foreground := "red" ]
    tagTable <- Gtk.textBufferGetTagTable buffer
    Gtk.textTagTableAdd tagTable tagRed
    
    (startIter, endIter) <-  #getBounds buffer
    text <- Gtk.textBufferGetText buffer startIter endIter True
    let wordPos = T.breakOn word text
    if T.null (snd wordPos) -- Verifica se encontrou a palavra
        then return () -- Se não encontrou, não faz nada
        else do
            let startOffset = fromIntegral (T.length (fst wordPos)) :: Int32
            let wordLength = fromIntegral (T.length word) :: Int32
            wordStartIter <- Gtk.textBufferGetIterAtOffset buffer startOffset
            wordEndIter <- Gtk.textBufferGetIterAtOffset buffer (startOffset + wordLength)
            Gtk.textBufferApplyTag buffer tagRed wordStartIter wordEndIter

app :: IO ()
app = do
    Gtk.init Nothing

    win <- Gtk.windowNew Gtk.WindowTypeToplevel

    Gtk.setWindowTitle win "Haskpad++"
    Gtk.setWindowDefaultHeight win 480
    Gtk.setWindowDefaultWidth win 640
    Gtk.setWindowWindowPosition win Gtk.WindowPositionCenter

    grid <- new Gtk.Grid []
    #add win grid
    
    btnBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ]
    btnNew <- new Gtk.Button [#label := "new"]
    btnSave <- new Gtk.Button [#label := "save"]
    
    #add btnBox btnNew
    #add btnBox btnSave

    -- Cria um TextBuffer e um TextView para editar texto
    textView <- new Gtk.TextView []
    buffer <- Gtk.textViewGetBuffer textView
    Gtk.textViewSetWrapMode textView Gtk.WrapModeWord -- Quebra de linha automática
    Gtk.widgetSetHexpand textView True
    Gtk.widgetSetVexpand textView True

    -- os numeros sao coluna linha #linhas #colunas que ocupa
    #attach grid btnBox   0 0 1 1
    #attach grid textView 0 1 1 1

    -- Ações dos botões

    on btnSave #clicked $ do
        
        filePath <- openSaveDialog
        print(filePath)
        buffer <- #getBuffer textView
        (startIter, endIter) <- #getBounds buffer 
        content <- Gtk.textBufferGetText buffer startIter endIter True
        
        -- Destacar palavra "teste" em vermelho
        highlightWord "batata" buffer
        
        let stringContent = T.unpack content
        writeFile filePath stringContent 

    Gtk.onWidgetDestroy win Gtk.mainQuit
    #showAll win
    Gtk.main

main :: IO ()
main = app
