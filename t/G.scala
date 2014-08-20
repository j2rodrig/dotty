package test

import annotation.{StaticAnnotation, TypeConstraint}

class readonly extends TypeConstraint {}
class polyread extends TypeConstraint {}
class mutable extends TypeConstraint {}


trait G {
	var ro: AnyRef @readonly
	var pr: AnyRef @polyread
	var mu: AnyRef @mutable
	
	def muDef = mu
	def prDef = pr
	def roDef = ro
	
	def d0(): AnyRef @mutable = ro     // should error
	@mutable def d0p() = ro            // should error
	
	def d1(): AnyRef @readonly = ro
	@readonly def d1p() = ro

	@mutable def e0() = d0()
	@mutable def e1() = d1()           // should error
	@readonly def e10() = d0()
	@readonly def e11() = d1()
	@mutable def e0p() = d0p()
	@mutable def e1p() = d1p()         // should error

	@polyread def f0pr(): AnyRef = ro  // should error
	@readonly def f0ro(): AnyRef = ro

	def dd0()(): AnyRef = d0()
	def dd1()(): AnyRef = d1()         // should error
	def dd10()(): AnyRef @polyread = e10()   // should error
	def dd11()(): AnyRef @readonly = e11()
}