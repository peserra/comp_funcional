{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import Data.Int (Int32)

import Idiom

getCaminhoArquivo :: Gtk.Entry -> IO String
getCaminhoArquivo entry = do
    entryText <- Gtk.entryGetText entry
    let inText = T.unpack entryText
    let caminhoPasta = "./TextosInput/"
    return (caminhoPasta ++ inText ++ ".txt")


salvaArquivo :: FilePath -> String -> IO ()
salvaArquivo = writeFile 


-- cria uma janela de salvamento
abreJanelaSave txtBuffer = do
    w <- Gtk.windowNew Gtk.WindowTypeToplevel
    Gtk.setWindowTitle w "File Name"
    Gtk.setWindowDefaultHeight w 35
    Gtk.setWindowDefaultWidth w 350
    Gtk.setWindowWindowPosition w Gtk.WindowPositionCenter

    -- Cria uma caixa horizontal para organizar o Entry e o Botão lado a lado
    hbox <- Gtk.boxNew Gtk.OrientationHorizontal 10
    Gtk.containerAdd w hbox

    -- Cria um campo de entrada (Entry)
    entry <- Gtk.entryNew
    Gtk.boxPackStart hbox entry True True 0  -- O campo de entrada preenche o espaço disponível

    -- Cria um botão ao lado do Entry
    okBtn <- Gtk.buttonNewWithLabel "Ok"

    Gtk.boxPackStart hbox okBtn False False 0  -- O botão tem tamanho fixo e fica ao lado
    #showAll w

    on okBtn #clicked $ do
        caminho <- getCaminhoArquivo entry 
        salvaArquivo caminho txtBuffer
        #destroy w
    return ()


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
    -- buffer <- Gtk.textViewGetBuffer textView
    Gtk.textViewSetWrapMode textView Gtk.WrapModeWord -- Quebra de linha automática
    Gtk.widgetSetHexpand textView True
    Gtk.widgetSetVexpand textView True

    -- os numeros sao coluna linha #linhas #colunas que ocupa
    #attach grid btnBox   0 0 1 1
    #attach grid textView 0 1 1 1

    -- Ações dos botões

    on btnSave #clicked $ do

        buffer <- #getBuffer textView
        (startIter, endIter) <- #getBounds buffer 
        content <- Gtk.textBufferGetText buffer startIter endIter True
        let bufferText = T.unpack content
        -- -- Destacar palavra "teste" em vermelho
        -- highlightWord "batata" buffer
        
        erradas <- Idiom.findWrongWordsList bufferText
        print erradas
        abreJanelaSave bufferText


    Gtk.onWidgetDestroy win Gtk.mainQuit
    #showAll win
    Gtk.main

main :: IO ()
main = app
