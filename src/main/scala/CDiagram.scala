import scala.util.parsing.combinator._
import java.io._
import scala.collection.mutable._

/*
ICustomer, IXML [Customer] 
IOrder, IXML [Order] 

[Customer] persitence -> IPersistence [Persitence]

*/
case class Component(name: String)
case class Interface(name: String)
case class Relation(name: String)

case class InterfaceReference(component: Component, interface: Interface)
case class RelationReference(component: Component, relation: Relation)

case class ComponentRelation(rref: RelationReference, iref: InterfaceReference)


class CDiagram extends JavaTokenParsers  {
	var relationsPerComponent = new ListMap[Component, Set[Relation]]
	var interfacesPerComponent = new ListMap[Component, Set[Interface]]
	var componentRelations = new ListMap[Component, Set[ComponentRelation]]
	
	def identifier = """[a-zA-Z]+""".r
	
	def component = "[" ~> identifier <~ "]" ^^ Component
	def interface = identifier ^^ Interface
	def relation = identifier ^^ Relation
	
	def interfaceRefs = repsep(interface, ",") ~ component ^^ { case is ~ c => 
		is.map { i: Interface =>
			interfacesPerComponent.getOrElseUpdate(c, new HashSet[Interface]) += i
			InterfaceReference(c, i) 
		}
	}
	
	def relationDef = component ~ relation ^^ { case c ~ r => 
		relationsPerComponent.getOrElseUpdate(c, new HashSet[Relation]) += r
		RelationReference(c, r) 
	}
	
	def expr = opt(relationDef <~ "->") ~ interfaceRefs ^^ { 
		case relationRef ~ interfaceRefs =>
			interfaceRefs.map { interfaceRef => 
				relationRef match {
					case Some(relationRef) =>
						val c = relationRef.component
						val cr = ComponentRelation(relationRef, interfaceRef)
						val s = componentRelations.getOrElseUpdate(c, new HashSet[ComponentRelation]) += cr
						componentRelations += c -> s
					case None => 
				}
			}
	}
	def multilineExpr = rep(expr)
	
}

object CDiagram {
	
	def outputDot(diagram: CDiagram): String = {
		val components = diagram.interfacesPerComponent.keySet ++ diagram.relationsPerComponent.keySet
		
		"""digraph G {
			|  node [shape=record];
			|%s
			|}	
		""".stripMargin.format(
			components.map { c: Component => 
				"""  %s[label="{%s} | %s"];
				|  %s""".stripMargin.format(
					c.name,
					diagram.interfacesPerComponent.getOrElse(c, new HashSet[Interface]).map { i: Interface => 
					 	"<%s> %s".format(i.name, i.name)
					 }.mkString("|"),
					"\\<\\<component\\>\\>\\n" + c.name,
					diagram.componentRelations.getOrElse(c, new HashSet[ComponentRelation]).map { cr: ComponentRelation =>
						"""%s -> %s:%s [label="%s"];""".format(
							c.name,
							cr.iref.component.name,
							cr.iref.interface.name,
							cr.rref.relation.name
					 	)
					}.mkString("\n  ")
				)
			}.mkString("\n")
		)
	}
	
}

object CDIagramFileRunner {
	def main(args: Array[String]) {
		val c = new CDiagram
		val a = c.parseAll(c.multilineExpr, new StringReader("""
		ICustomer, IXML [Customer]
		IOrder, IXML [Order]

		[Customer] persitence -> IPersistence [Persitence]
		[Customer] orders -> IOrder [Order]
		[XML] convertCusts -> IXML [Customer]
		[XML] convertOrders -> IXML [Order]
		"""))

		println(outputDot(c))
	}

}