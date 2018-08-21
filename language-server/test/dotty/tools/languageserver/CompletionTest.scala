package dotty.tools.languageserver

import org.junit.Test
import org.eclipse.lsp4j.CompletionItemKind

import dotty.tools.languageserver.util.Code._

class CompletionTest {

  @Test def completion0: Unit = {
    code"class Foo { val xyz: Int = 0; def y: Int = xy$m1 }".withSource
      .completion(m1, Set(("xyz", CompletionItemKind.Field, "Int")))
  }

  @Test def completionOnImport: Unit = {
    code"""import java.io.FileDescriptor
           trait Foo { val x: FileDesc$m1 }""".withSource
      .completion(m1, Set(("FileDescriptor", CompletionItemKind.Class, "Object{...}")))
  }

  @Test def completionOnRenamedImport: Unit = {
    code"""import java.io.{FileDescriptor => AwesomeStuff}
           trait Foo { val x: Awesom$m1 }""".withSource
      .completion(m1, Set(("AwesomeStuff", CompletionItemKind.Class, "Object{...}")))
  }

  @Test def completionOnRenamedImport2: Unit = {
    code"""import java.util.{HashMap => MyImportedSymbol}
           trait Foo {
             import java.io.{FileDescriptor => MyImportedSymbol}
             val x: MyImp$m1
           }""".withSource
      .completion(m1, Set(("MyImportedSymbol", CompletionItemKind.Class, "Object{...}")))
  }
}
