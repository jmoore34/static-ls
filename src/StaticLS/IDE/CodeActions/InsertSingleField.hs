module StaticLS.IDE.CodeActions.InsertSingleField (codeAction) where

import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.IDE.CodeActions.Utils (prefer, quickFix, insertBelow)

codeAction ::
  LSP.TextDocumentIdentifier ->
  LSP.Diagnostic ->
  T.Text ->
  Int ->
  LSP.CodeAction
codeAction tdi diag missingField leadingSpaces =
   prefer . quickFix tdi diag "Insert missing field" (insertBelow diag._range) $
      (T.replicate leadingSpaces " ") <> ", " <> missingField
