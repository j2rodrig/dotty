//package test

import annotation._

//import scala.annotation.Annotation
//class READONLY extends Annotation {}

/*import annotation.{StaticAnnotation, TypeConstraint}

class readonly extends TypeConstraint {}
class polyread extends TypeConstraint {}
class mutable extends TypeConstraint {}*/


package P.Q {
	object v { var m = null }
	package R {
		object w { var n = null }
		class C {
			var x = null
			class TempD {
			}
			val cd = new C.this.TempD
			def F = {
				var y = null
				class D {
					var z = null
					def G = {
						var a = null
						def H = {
							a = null
							z = null
							this.z = null
							D.this.z = null
							y = null
							x = null
							C.this.x = null
							w.n = null
							R.w.n = null
							P.Q.R.w.n = null
							
							val c = new C
							val d = new D
							val rc = new R.C
							
							def D(k:Int)(j:Int) = {}
							val dj = D (10) _
							dj(20)
						}
					}
				}
				class E extends D {
					def b = 0
				}
				val e: D = new E
				val f = e.asInstanceOf[E].b
				
				val arr = new Array[Int](5)
				arr(0) = 2
				
				def __f(): (() => Unit) @readonly = { () => }
			}
		}
	}
}


// def __f(): (() => Unit) @readonly = { () => }   // OK: wrapping a function type in an annotation
/*DefDef(Modifiers(<method>,,List()),__f,List(),List(List()),
	Annotated(
		Apply(Select(New(Ident(readonly)),<init>),List()),
		AppliedTypeTree(TypeTree[TypeRef(ThisType(module class scala),Function0)],List(Ident(Unit)))),
	Block(List(),Block(List(DefDef(Modifiers(<synthetic>,,List()),$anonfun,List(),List(List()),TypeTree[TypeRef(ThisType(module class
scala),Unit)],Block(List(),Literal(Constant(()))))),Closure(List(),Ident($anonfun),EmptyTree))))),Literal(Constant(())))))))))))))*/

// vars have a modifier "mutable"

/*

//
// val arr = new Array[Int](5)
//
ValDef(Modifiers(,,List()),arr,
	TypeTree[RefinedType(TypeRef(ThisType(module class scala),Array),scala$Array$$T,
		TypeAlias(TypeRef(TermRef(ThisType(module class <root>),scala),Int)))],
	Apply(
		TypeApply(
			Select(New(TypeTree[TypeRef(TermRef(ThisType(module class <root>),scala),Array)]),<init>),
			List(TypeTree[TypeRef(TermRef(ThisType(module class <root>),scala),Int)])
			),
		List(Literal(Constant(5)))
	)
)

// arr[0] = 2
//
Apply(Select(Ident(arr),update),List(Literal(Constant(0)), Literal(Constant(2))))


// for var-as-field
ValDef(Modifiers(mutable,,List()),VVAARR,TypeTree[TypeRef(ThisType(module class scala),Int)],Literal(Constant(2))),
DefDef(Modifiers(<accessor> mutable,,List()),VVAARR_$eq,List(),List(List(ValDef(Modifiers(<param>
	<synthetic>,,List()),x$1,TypeTree[TypeRef(ThisType(module class scala),Int)],EmptyTree)))
	,TypeTree[TypeRef(ThisType(module class scala),Unit)],Literal(Constant(())))

// ordinary object
PackageDef(Ident(test),List(TypeDef(
	Modifiers(,,List()),encl,Template(
		DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),
		List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),List())),
		ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),
		List(
			ValDef(Modifiers(final module <stable>,,List()),OOOO,TypeTree[TypeRef(ThisType(class encl),OOOO$)],
				Apply(Select(New(TypeTree[TypeRef(ThisType(class encl),OOOO$)]),<init>),List())),
			TypeDef(Modifiers(final
				module,,List()),OOOO$,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module class
				scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
				lang),Object)]),<init>),List())),ValDef(Modifiers(private <selfname>,,List()),_,TypeTree[TermRef(ThisType(class
				encl),OOOO)],EmptyTree),List(ValDef(Modifiers(,,List()),YYYY,TypeTree[TypeRef(ThisType(module class
				test),encl)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class test),encl)]),<init>),List()))))),
			DefDef(Modifiers(<method>,,List()),TTTT,List(),List(List()),TypeTree[TypeRef(ThisType(module class
				test),encl)],Select(Ident(OOOO),YYYY))
		)
	)
)))

// object with @READONLY
PackageDef(Ident(test),List(TypeDef(Modifiers(,,List()),READONLY,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),Type
Tree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(TermRef(TermRef(ThisType(module
class <root>),scala),annotation),Annotation)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List())),
TypeDef(
	Modifiers(,,List()),encl,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module
	class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
	lang),Object)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List(
	ValDef(
		Modifiers(final module <stable>,,List(Apply(Select(New(Ident(READONLY)),<init>),List()))),
		B,
		TypeTree[TypeRef(ThisType(class encl),B$)],
		Apply(Select(New(TypeTree[TypeRef(ThisType(class encl),B$)]),<init>),List())
	),
	TypeDef(Modifiers(final module,,List(Apply(Select(New(Ident(READONLY)),<init>),List()))),B$,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),
		TypeTree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
		lang),Object)]),<init>),List())),ValDef(Modifiers(private <selfname>,,List()),_,TypeTree[TermRef(ThisType(class
		encl),B)],EmptyTree),List())
	)
)))))

// For:
class CCCC (var VVV: AnyRef) {
	VVV = null
}
PackageDef(Ident(<empty>),List(TypeDef(Modifiers(,,List()),encl,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeT
ree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
lang),Object)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List(TypeDef(Modifiers(,,List()),CCCC,
Template(
	DefDef(
		Modifiers(,,List()),
		<init>,
		List(),
		List(List(ValDef(Modifiers(<param>,,List()),VVV,TypeTree[TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef)],
			EmptyTree))),
		TypeTree[TypeRef(ThisType(module class scala),Unit)],
		EmptyTree),
	List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),List())),
	ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),
	List(
		ValDef(Modifiers(mutable <paramaccessor>,,List()),VVV,
			TypeTree[TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef)],EmptyTree),
		DefDef(
			Modifiers(<accessor> mutable <paramaccessor>,,List()),
			VVV_$eq,
			List(),
			List(List(ValDef(Modifiers(<param> <synthetic>,,List()),x$1,TypeTree[TypeRef(TermRef(ThisType(module class
				<root>),scala),AnyRef)],EmptyTree))),
			TypeTree[TypeRef(ThisType(module class scala),Unit)],
			EmptyTree),
		Assign(Ident(VVV),Literal(Constant(null)))
	)
)
))))))

// anonymous function assigned to val
PackageDef(Ident(<empty>),List(TypeDef(Modifiers(,,List()),encl,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeT
ree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
lang),Object)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List(
	ValDef(Modifiers(,,List()),V____V,TypeTree[RefinedType(TypeRef(ThisType(module class scala),Function0), scala$Function0$$R, CoTypeAlias(TypeRef(ThisType(module class
		scala),Unit)))],
	Block(
		List(
			DefDef(Modifiers(<synthetic>,,List()),$anonfun,List(),List(List()),TypeTree[TypeRef(ThisType(module
				class scala),Unit)],
				Literal(Constant(()))
			)
		),
		Closure(List(),Ident($anonfun),EmptyTree)
	)
))))))


// anonymous with parameter
PackageDef(Ident(<empty>),List(TypeDef(Modifiers(,,List()),encl,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeT
ree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
lang),Object)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List(ValDef(Modifiers(,,List()),V____V,Typ
eTree[RefinedType(RefinedType(TypeRef(ThisType(module class scala),Function1), scala$Function1$$T1,
ContraTypeAlias(TypeRef(TermRef(ThisType(module class <root>),scala),Any))), scala$Function1$$R,
CoTypeAlias(TypeRef(TermRef(ThisType(module class
<root>),scala),Any)))],Block(List(DefDef(Modifiers(<synthetic>,,List()),$anonfun,List(),List(List(ValDef(Modifiers(<param>,,List()),
p,TypeTree[TypeRef(TermRef(ThisType(module class <root>),scala),Any)],EmptyTree))),TypeTree[TypeRef(TermRef(ThisType(module class
<root>),scala),Any)],Ident(p))),Closure(List(),Ident($anonfun),EmptyTree))))))))


// passing a function as a parameter
PackageDef(Ident(test),List(TypeDef(Modifiers(,,List()),encl,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree
[TypeRef(ThisType(module class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
lang),Object)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List(DefDef(Modifiers(<method>,,List()),FF
FFFF,List(),List(List(ValDef(Modifiers(<param>,,List()),fffff,TypeTree[RefinedType(TypeRef(ThisType(module class scala),Function0),
scala$Function0$$R, CoTypeAlias(TypeRef(TermRef(ThisType(module class
<root>),scala),Unit)))],EmptyTree))),TypeTree[TypeRef(ThisType(module class scala),Unit)],Block(List(),Literal(Constant(())))),
DefDef(Modifiers(<method>,,List()),g,List(),List(List()),TypeTree[TypeRef(ThisType(module class
scala),Unit)],Block(List(),Literal(Constant(())))),
Apply(Ident(FFFFFF),List(Block(
	List(DefDef(Modifiers(<synthetic>,,List()),$anonfun,List(),List(List()),TypeTree[TypeRef(TermRef(This
		Type(module class <root>),scala),Unit)],Apply(Ident(g),List()))),
	Closure(List(),Ident($anonfun),EmptyTree)))))))))

// passing an anonymous function as a parameter
PackageDef(Ident(test),List(TypeDef(Modifiers(,,List()),encl,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree
[TypeRef(ThisType(module class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
lang),Object)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),
List(DefDef(Modifiers(<method>,,List()),FFFFFF,List(),List(
		List(
			ValDef(Modifiers(<param>,,List()),fffff,TypeTree[RefinedType(TypeRef(ThisType(module class scala),Function0),
			scala$Function0$$R, CoTypeAlias(TypeRef(TermRef(ThisType(module class
			<root>),scala),Unit)))],EmptyTree)
		)
		),TypeTree[TypeRef(ThisType(module class scala),Unit)],
		Block(List(),Literal(Constant(())))
	),
	Apply(Ident(FFFFFF),List(Block(
		List(),Block(
			List(DefDef(Modifiers(<synthetic>,,List()),$anonfun,List(),List(List()),
				TypeTree[TypeRef(TermRef(ThisType(module class <root>),scala),Unit)],
				Block(List(),Literal(Constant(()))))),
			Closure(List(),Ident($anonfun),EmptyTree)))))
)))))



// lazy val as a local variable
PackageDef(Ident(test),List(TypeDef(Modifiers(,,List()),encl,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree
[TypeRef(ThisType(module class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
lang),Object)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List(
DefDef(Modifiers(<method>,,List()),f,
	List(),List(List()),TypeTree[TypeRef(ThisType(module class scala),Unit)],Block(
	List(ValDef(Modifiers(,,List()),VVVV$lzy1,TypeTree[TypeRef(ThisType(module class
			runtime),LazyRef)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class
			runtime),LazyRef)]),<init>),List(Block(List(DefDef(Modifiers(,,List()),$anonfun,List(),List(List()),TypeTree[TypeRef(ThisType(module
			 class test),encl)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class
			test),encl)]),<init>),List()))),Closure(List(),Ident($anonfun),EmptyTree))))),
		DefDef(Modifiers(,,List()),VVVV,List(),List(List()),TypeTree[TypeRef(ThisType(module class
			test),encl)],Apply(Select(Ident(VVVV$lzy1),value),List()))
	),Literal(Constant(())))))))))



// lazy object
PackageDef(Ident(test),List(TypeDef(Modifiers(,,List()),encl,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree
[TypeRef(ThisType(module class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
lang),Object)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),
List(ValDef(Modifiers(,,List()),OOOO$lzy1,
	TypeTree[TypeRef(ThisType(class encl),OOOO$)],Literal(Constant(null))),
	ValDef(Modifiers(,,List()),OOOObitmap$1,TypeTree[TypeRef(ThisType(module class scala),Boolean)],Literal(Constant(false))),
	DefDef(Modifiers(,,List()),OOOO,List(),List(List()),TypeTree[TypeRef(ThisType(class
	encl),OOOO$)],If(Ident(OOOObitmap$1),Ident(OOOO$lzy1),Block(List(Assign(Ident(OOOObitmap$1),Literal(Constant(true))),
	Assign(Ident(OOOO$lzy1),Apply(Select(New(TypeTree[TypeRef(ThisType(class encl),OOOO$)]),<init>),List()))),Ident(OOOO$lzy1)))),
	TypeDef(Modifiers(final
	module,,List()),OOOO$,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module class
	scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
	lang),Object)]),<init>),List())),ValDef(Modifiers(private <selfname>,,List()),_,TypeTree[TermRef(ThisType(class
	encl),OOOO)],EmptyTree),List(ValDef(Modifiers(,,List()),YYYY,TypeTree[TypeRef(ThisType(module class
	test),encl)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class test),encl)]),<init>),List()))))),
	DefDef(Modifiers(<method>,,List()),TTTT,List(),List(List()),TypeTree[TypeRef(ThisType(module class
	test),encl)],Select(Ident(OOOO),YYYY))
)))))

// lazy val with READONLY
PackageDef(Ident(test),List(TypeDef(Modifiers(,,List()),READONLY,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),Type
Tree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(TermRef(TermRef(ThisType(module
class <root>),scala),annotation),Annotation)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List())),
TypeDef(Modifiers(,,List()),encl,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module
class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
lang),Object)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List(ValDef(Modifiers(,,List()),XXXX$lzy1,
TypeTree[TypeRef(ThisType(module class scala),Int)],Literal(Constant(0))),
ValDef(Modifiers(,,List()),XXXXbitmap$1,TypeTree[TypeRef(ThisType(module class scala),Boolean)],Literal(Constant(false))),
DefDef(Modifiers(,,List(Apply(Select(New(Ident(READONLY)),<init>),List()))),XXXX,List(),List(List()),TypeTree[TypeRef(ThisType(modul
e class scala),Int)],If(Ident(XXXXbitmap$1),Ident(XXXX$lzy1),Block(List(Assign(Ident(XXXXbitmap$1),Literal(Constant(true))),
Assign(Ident(XXXX$lzy1),Literal(Constant(60)))),Ident(XXXX$lzy1)))))))))

// lazy val with READONLY type
PackageDef(Ident(test),List(TypeDef(Modifiers(,,List()),READONLY,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),Type
Tree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(TermRef(TermRef(ThisType(module
class <root>),scala),annotation),Annotation)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List())),
TypeDef(Modifiers(,,List()),encl,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module
class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
lang),Object)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List(ValDef(Modifiers(,,List()),XXXX$lzy1,
TypeTree[AnnotatedType(ConcreteAnnotation(Apply(Select(New(Ident(READONLY)),<init>),List())),TypeRef(TermRef(ThisType(module class
<root>),scala),Int))],Literal(Constant(0))), ValDef(Modifiers(,,List()),XXXXbitmap$1,TypeTree[TypeRef(ThisType(module class
scala),Boolean)],Literal(Constant(false))),
DefDef(Modifiers(,,List()),XXXX,List(),List(List()),TypeTree[AnnotatedType(ConcreteAnnotation(Apply(Select(New(Ident(READONLY)),<ini
t>),List())),TypeRef(TermRef(ThisType(module class
<root>),scala),Int))],If(Ident(XXXXbitmap$1),Ident(XXXX$lzy1),Block(List(Assign(Ident(XXXXbitmap$1),Literal(Constant(true))),
Assign(Ident(XXXX$lzy1),Literal(Constant(60)))),Ident(XXXX$lzy1)))))))))


// case class
PackageDef(Ident(test),List(TypeDef(Modifiers(,,List()),encl,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree
[TypeRef(ThisType(module class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
lang),Object)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),
List(
	TypeDef(Modifiers(case,,List()),OOOO,
		Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module class
		scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
		lang),Object)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List(ValDef(Modifiers(,,List()),YYYY,TypeT
		ree[TypeRef(ThisType(module class test),encl)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class
		test),encl)]),<init>),List())), DefDef(Modifiers(<synthetic>,,List()),copy,List(),List(List()),TypeTree[TypeRef(ThisType(class
		encl),OOOO)],Apply(Select(New(TypeTree[TypeRef(ThisType(class encl),OOOO)]),<init>),List())),
		DefDef(Modifiers(<synthetic>,,List()),isDefined,List(),List(List()),TypeTree[TypeRef(ThisType(module class
		scala),Boolean)],Literal(Constant(true))),
		DefDef(Modifiers(<synthetic>,,List()),productArity,List(),List(List()),TypeTree[TypeRef(ThisType(module class
		scala),Int)],Literal(Constant(0)))))),
	ValDef(Modifiers(final module <synthetic>
		<stable>,,List()),OOOO,TypeTree[TypeRef(ThisType(class encl),OOOO$)],Apply(Select(New(TypeTree[TypeRef(ThisType(class
		encl),OOOO$)]),<init>),List())),
	TypeDef(Modifiers(final module,,List()), OOOO$, Template(
		DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module class
		scala),Unit)],EmptyTree),
		List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),List()),
			TypeTree[RefinedType(TypeRef(ThisType(module class scala),Function0), scala$Function0$$R, CoTypeAlias(TypeRef(ThisType(class
			encl),OOOO)))]),
		ValDef(Modifiers(private <selfname>,,List()),_,TypeTree[TermRef(ThisType(class encl),OOOO)],EmptyTree),
		List(DefDef(Modifiers(<synthetic>,,List()),apply,List(),List(List()),TypeTree[TypeRef(ThisType(module class
			OOOO$),scala$Function0$$R)],Apply(Select(New(TypeTree[TypeRef(ThisType(class encl),OOOO)]),<init>),List())),
			DefDef(Modifiers(<synthetic>,,List()),unapply,List(),List(List(ValDef(Modifiers(<param>
			<synthetic>,,List()),x$1,TypeTree[TypeRef(ThisType(class encl),OOOO)],EmptyTree))),TypeTree[TypeRef(ThisType(module class
			scala),Boolean)],Literal(Constant(true)))
		)
	))
)
))))

// case class with READONLY
PackageDef(Ident(test),List(TypeDef(Modifiers(,,List()),READONLY,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),Type
Tree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(TermRef(TermRef(ThisType(module
class <root>),scala),annotation),Annotation)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List())),
TypeDef(Modifiers(,,List()),encl,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module
class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
lang),Object)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List(TypeDef(Modifiers(case,,List(Apply(Se
lect(New(Ident(READONLY)),<init>),List()))),OOOO,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(Thi
sType(module class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
lang),Object)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List(DefDef(Modifiers(<synthetic>,,List())
,copy,List(),List(List()),TypeTree[TypeRef(ThisType(class encl),OOOO)],Apply(Select(New(TypeTree[TypeRef(ThisType(class
encl),OOOO)]),<init>),List())),
DefDef(Modifiers(<synthetic>,,List()),isDefined,List(),List(List()),TypeTree[TypeRef(ThisType(module class
scala),Boolean)],Literal(Constant(true))),
DefDef(Modifiers(<synthetic>,,List()),productArity,List(),List(List()),TypeTree[TypeRef(ThisType(module class
scala),Int)],Literal(Constant(0)))))), ValDef(Modifiers(final module <synthetic>
<stable>,,List()),OOOO,TypeTree[TypeRef(ThisType(class encl),OOOO$)],Apply(Select(New(TypeTree[TypeRef(ThisType(class
encl),OOOO$)]),<init>),List())), TypeDef(Modifiers(final
module,,List()),OOOO$,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module class
scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),List()),
TypeTree[RefinedType(TypeRef(ThisType(module class scala),Function0), scala$Function0$$R, CoTypeAlias(TypeRef(ThisType(class
encl),OOOO)))]),ValDef(Modifiers(private <selfname>,,List()),_,TypeTree[TermRef(ThisType(class
encl),OOOO)],EmptyTree),List(DefDef(Modifiers(<synthetic>,,List()),apply,List(),List(List()),TypeTree[TypeRef(ThisType(module class
OOOO$),scala$Function0$$R)],Apply(Select(New(TypeTree[TypeRef(ThisType(class encl),OOOO)]),<init>),List())),
DefDef(Modifiers(<synthetic>,,List()),unapply,List(),List(List(ValDef(Modifiers(<param>
<synthetic>,,List()),x$1,TypeTree[TypeRef(ThisType(class encl),OOOO)],EmptyTree))),TypeTree[TypeRef(ThisType(module class
scala),Boolean)],Literal(Constant(true)))))))))))



Just object encl:
PackageDef(Ident(test),List(ValDef(Modifiers(final module <stable>,,List()),encl,TypeTree[TypeRef(ThisType(module class
test),encl$)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class test),encl$)]),<init>),List())), TypeDef(Modifiers(final
module,,List()),encl$,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module class
scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
lang),Object)]),<init>),List())),ValDef(Modifiers(private <selfname>,,List()),_,TypeTree[TermRef(ThisType(module class
test),encl)],EmptyTree),List()))))

With object B inside class encl:
PackageDef(Ident(test),List(TypeDef(Modifiers(,,List()),encl,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[
TypeRef(ThisType(module class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
lang),Object)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List(ValDef(Modifiers(final module
<stable>,,List()),B,TypeTree[TypeRef(ThisType(class encl),B$)],Apply(Select(New(TypeTree[TypeRef(ThisType(class
encl),B$)]),<init>),List())), TypeDef(Modifiers(final
module,,List()),B$,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module class
scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
lang),Object)]),<init>),List())),ValDef(Modifiers(private <selfname>,,List()),_,TypeTree[TermRef(ThisType(class
encl),B)],EmptyTree),List())))))))


With case class B:
PackageDef(Ident(test),List(ValDef(Modifiers(final module <stable>,,List()),encl,TypeTree[TypeRef(ThisType(module class
test),encl$)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class test),encl$)]),<init>),List())), TypeDef(Modifiers(final
module,,List()),encl$,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module class
scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
lang),Object)]),<init>),List())),ValDef(Modifiers(private <selfname>,,List()),_,TypeTree[TermRef(ThisType(module class
test),encl)],EmptyTree),List(

TypeDef(
	Modifiers(case,,List()),B,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTre
	e[TypeRef(ThisType(module class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
	lang),Object)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List(DefDef(Modifiers(<synthetic>,,List()),
	copy,List(),List(List()),TypeTree[TypeRef(ThisType(module class encl$),B)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class
	encl$),B)]),<init>),List())), DefDef(Modifiers(<synthetic>,,List()),isDefined,List(),List(List()),TypeTree[TypeRef(ThisType(module
	class scala),Boolean)],Literal(Constant(true))),
	DefDef(Modifiers(<synthetic>,,List()),productArity,List(),List(List()),TypeTree[TypeRef(ThisType(module class
	scala),Int)],Literal(Constant(0)))))),

ValDef(Modifiers(final module <synthetic> <stable>,,List()),B,TypeTree[TypeRef(ThisType(module class encl$),B$)],
	Apply(Select(New(TypeTree[TypeRef(ThisType(module class encl$),B$)]),<init>),List())),

TypeDef(
	Modifiers(final module,,List()),B$,Template(
		DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module class
		scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),List()),
		TypeTree[RefinedType(TypeRef(ThisType(module class scala),Function0), scala$Function0$$R, CoTypeAlias(TypeRef(ThisType(module class
		encl$),B)))]),ValDef(Modifiers(private <selfname>,,List()),_,TypeTree[TermRef(ThisType(module class
		encl$),B)],EmptyTree),
		List(
			DefDef(Modifiers(<synthetic>,,List()),apply,List(),List(List()),TypeTree[TypeRef(ThisType(module class
			B$),scala$Function0$$R)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class encl$),B)]),<init>),List())),
			DefDef(Modifiers(<synthetic>,,List()),unapply,List(),List(List(ValDef(Modifiers(<param>
			<synthetic>,,List()),x$1,TypeTree[TypeRef(ThisType(module class encl$),B)],EmptyTree))),TypeTree[TypeRef(ThisType(module class
			scala),Boolean)],Literal(Constant(true)))
			)
		)
	)

)))))

*/
