class ULTerm

  # By default, we assume terms are irreducible,
  # not abstractions, and not values.
  # Subclasses which should have these properties
  # must override these methods.
  # (In our basic calculus with call-by-value semantics,
  # only applications are reducible and only abstractions
  # are values. This can be changed for different calculi/semantics.)
  def reduce; nil end
  def absBody; nil end
  def isValue?; false end
  
  # Shifting is just walking, where in the base case,
  # we either increment the variable by shiftAmount or
  # leave it alone.
  def shift(shiftAmount)
    # walk is an iterator.
    # The block tells us what to do with variables.
    walk(0) { |x,currentBinders|
      if x >= currentBinders
        ULVar.new(x+shiftAmount)
      else
        ULVar.new(x)
      end }
  end

  # Substitution is just walking, where we either
  # replace the variable, or leave it alone.
  def substitute(x,r)
    walk(0) { |y,currentBinders|
      if y == x + currentBinders
        r
      else
        ULVar.new(y)
      end }
  end
  def eval
    r = nil
    r_next = self
    # Keep reducing until it fails (reduce returns nil.)
    # This is the recommended "do...while" form in Ruby.
    loop do
      r = r_next
      r_next = r.reduce
      break unless r_next
    end

    return r
  end
end

class ULVar < ULTerm
  attr_reader :index

  # We require our variables are only indexed by integers.
  def initialize(i)
    unless i.is_a?(Integer)
      throw "Constructing a lambda term out of non-lambda terms"
    end
    @index = i
  end
  
  def walk(currentBinders,&block)
    # This is a variable. Run the code in &block.
    # (yield does this; it "yields" control to the block.)
    yield(@index, currentBinders)
  end

  def to_s
    @index.to_s
  end

  def ==(r); r.is_a?(ULVar) && r.index == @index end
end

class ULAbs < ULTerm
  attr_reader :t

  def initialize(t)
    unless t.is_a?(ULTerm)
      throw "Constructing a lambda term out of a non-lambda term"
    end
    @t = t
  end
  
  def walk(currentBinders,&block)
    # Increment the local variable counter within the variable binder.
    t = @t.walk(currentBinders+1,&block)
    ULAbs.new(t)
  end

  # Abstractions are an abstraction (of course),
  # with body @t,
  # and are also considered values.
  def absBody; @t end
  def isValue?; true end
  
  def to_s
    "lambda . " + @t.to_s
  end

  def ==(r); r.is_a?(ULAbs) && r.t == @t end
end

class ULApp < ULTerm
  attr_reader :t1
  attr_reader :t2

  def initialize(t1,t2)
    unless t1.is_a?(ULTerm) && t2.is_a?(ULTerm)
      throw "Constructing a lambda term out of non-lambda terms"
    end
    @t1 = t1; @t2 = t2
  end
  
  def walk(currentBinders,&block)
    t1 = @t1.walk(currentBinders,&block)
    t2 = @t2.walk(currentBinders,&block)
    ULApp.new(t1,t2)
  end

  # Applications can be reduced.
  def reduce
    if @t1.absBody && @t2.isValue?
      body = @t1.absBody
      (body.substitute(0,@t2.shift(1))).shift(-1)
    elsif @t1.isValue?
      r = @t2.reduce
      if r
        ULApp.new(@t1,r)
      else
        nil
      end
    else
      r = @t1.reduce
      if r
        ULApp.new(r,@t2)
      else
        nil
      end
    end
  end

  def to_s
    "(" + @t1.to_s + ") (" + @t2.to_s + ")" 
  end

  def ==(r); r.is_a?(ULApp) && r.t1 == @t1 && r.t2 == @t2 end
end

class STTerm end

class STVar < STTerm
  def initialize(index)
    unless index.is_a?(Integer)
      throw "Constructing a type out of non-types"
    end
	@index = index
  end
  
  def eraseTypes
	self.eraseTypes_helper(0)
  end
	
  def eraseTypes_helper(index)
    ULVar.new(self.index)
  end  
  
  def typecheck
	false
  end
  
  def filled_Var
	true
  end
	
  attr_reader :index
  attr_writer :index
end

class STAbs < STTerm
  def initialize(term1, term2)
	unless term1.is_a?(STType) && term2.is_a?(STTerm)
	  throw "Constructing a type out of non-types"
	end
	@term1 = term1
	@term2 = term2
  end
  
  def eraseTypes
	self.eraseTypes_helper(0)
  end
  
  def eraseTypes_helper(index)
    ULAbs.new(self.term2.eraseTypes_helper(index))
  end 
  
  def typecheck
    if self.term2.filled_Var == true
	  true
	else
	  self.term1 == self.term2.typeOf([]) && typecheck(self.term2)
	end
  end
  
  def typeOf(list1)
	unless list1 == []
	  throw "Getting type with a non-empty environment"
	end
	if self.term2.filled_Var == true
	  STFun.new(self.term1, self.term1)
	else
	  STFun.new(self.term1, self.term2.typeOf([]))
	end
  end
  
  def filled_Var
	self.term2.filled_Var
  end
	
  attr_reader :term1
  attr_writer :term1
  attr_reader :term2
  attr_writer :term2
end

class STApp < STTerm
  def initialize(term1, term2)
	unless term1.is_a?(STTerm) && term2.is_a?(STTerm)
	  throw "Constructing a type out of non-types"	
	end
	@term1 = term1
	@term2 = term2
  end
  
  def eraseTypes
	self.eraseTypes_helper(0)
  end
  
  def eraseTypes_helper(index)
    ULApp.new(self.term1.eraseTypes_helper(index), self.term2.eraseTypes_helper(index))
  end 
  
  def typecheck
    unless self.term1.is_a?(STAbs)
	  false
	end
	self.term1.term1 == self.term2.typeOf([]) && self.term1.typecheck && self.term2.typecheck
  end
  
  def typeOf(list1)
	unless list1 == []
	  throw "Getting type with a non-empty environment"
	end
	STFun.new(self.term1.typeOf([]), self.term2.typeOf([]))
  end
  
  def filled_Var
    self.term1.filled_Var && self.term2.filled_Var
  end
  
  def evaluation
    self.term1.evaluation + self.term2.evaluation
  end
	
  attr_reader :term1
  attr_writer :term1
  attr_reader :term2
  attr_writer :term2
end

class STZero < STTerm
  def typecheck
    true
  end
  
  def eraseTypes
	self.eraseTypes_helper(0)
  end
  
  def eraseTypes_helper(index)
    ULAbs.new(ULAbs.new(ULVar.new(0)))
  end
  
  def typeOf(list1)
    unless list1 == []
	  throw "Getting type with a non-empty environment"
	end
	STNat.new
  end
  
  def filled_Var
	false
  end
  
  def evaluation
    0.to_s
  end
end

class STSuc < STTerm
  def initialize(term)
	unless term.is_a?(STTerm)
      throw "Constructing a type out of non-types"
    end
	@term = term
  end
  
  def eraseTypes
	self.eraseTypes_helper(0)
  end
  
  def eraseTypes_helper(index)
    index = index + 3
    ULApp.new(ULAbs.new(ULAbs.new(ULAbs.new(ULApp.new(ULVar.new(index - 2),ULApp.new(ULApp.new(ULVar.new(index - 1),ULVar.new(index - 2)),ULVar.new(index - 3)))))), self.term.eraseTypes_helper(index))
  end
  
  def typecheck
    self.term.typeOf([]) == STNat.new && self.term.typecheck
  end
  
  def typeOf(list1)
    unless list1 == []
	  throw "Getting type with a non-empty environment"
	end
	STNat.new
  end
  
  def filled_Var
	self.term.filled_Var
  end
  
  def evaluation
    (self.term.evaluation + 1).to_s
  end
	
  attr_reader :term
  attr_writer :term
end

class STIsZero < STTerm
  def initialize(term)
  	unless term.is_a?(STTerm)
      throw "Constructing a type out of non-types"
    end
	@term = term
  end
  
  def eraseTypes
	self.eraseTypes_helper(0)
  end
  
  def eraseTypes_helper(index)
	index = index + 2
    ULApp.new(ULAbs.new(ULApp.new(ULApp.new(ULVar.new(index - 2), ULAbs.new(ULAbs.new(ULAbs.new(ULVar.new(index - 2))))),ULAbs.new(ULAbs.new(ULVar.new(index - 1))))), self.term.eraseTypes_helper(index))
  end
  
  def typecheck
    self.term.typeOf([]) == STNat.new && self.term.typecheck
  end
  
  def typeOf(list1)
    unless list1 == []
	  throw "Getting type with a non-empty environment"
	end
	STBool.new
  end
  
  def filled_Var
	self.term.filled_Var
  end
  
  def evaluation
    false.to_s
  end
	
  attr_reader :term
  attr_writer :term
end

class STTrue < STTerm
  def typecheck
    true
  end
  
  def eraseTypes
	self.eraseTypes_helper(0)
  end
  
  def eraseTypes_helper(index)
    ULAbs.new(ULAbs.new(ULVar.new(1)))
  end

  def typeOf(list1)
    unless list1 == []
	  throw "Getting type with a non-empty environment"
	end
	STBool.new
  end
  
  def filled_Var
	false
  end
  
  def evaluation
    true.to_s
  end
end

class STFalse < STTerm
  def typecheck
    true
  end

  def eraseTypes
	self.eraseTypes_helper(0)
  end
  
  def eraseTypes_helper(index)
    ULAbs.new(ULAbs.new(ULVar.new(0)))
  end
  
  def typeOf(list1)
    unless list1 == []
	  throw "Getting type with a non-empty environment"
	end
	STBool.new
  end
  def filled_Var
	false
  end
  
  def evaluation
    false.to_s
  end
end

class STTest < STTerm
  def initialize(term1, term2, term3)
  	unless term1.is_a?(STTerm) && term2.is_a?(STTerm) && term3.is_a?(STTerm)
      throw "Constructing a type out of non-types"
    end
	@term1 = term1
	@term2 = term2
	@term3 = term3
  end
  
  def eraseTypes
	self.eraseTypes_helper(0)
  end
  
  def eraseTypes_helper(index)
    index = index + 3
    ULApp.new(ULApp.new(ULApp.new(ULAbs.new(ULAbs.new(ULAbs.new(ULApp.new(ULApp.new(ULVar.new(index - 1),ULVar.new(index - 2)),ULVar.new(index - 3))))), term1.eraseTypes_helper(index)), term2.eraseTypes_helper(index)), term3.eraseTypes_helper(index))
  end
  
  def typecheck
    self.term1.typeOf([]) == STBool.new && @term2.typeOf([]) == @term3.typeOf([]) && @term2.typeOf([]) == self.typeOf([]) && @term1.typecheck && @term2.typecheck && @term3.typecheck
  end
  
  def typeOf(list1)
    unless list1 == []
	  throw "Getting type with a non-empty environment"
	end
	self.term2.typeOf([])
  end
  
  def filled_Var
	self.term2.filled_Var && self.term3.filled_Var
  end
  
  def evaluation
    (evaluation(self.term2) == evaluation(self.term3)).to_s
  end
	
  attr_reader :term1
  attr_writer :term1
  attr_reader :term2
  attr_writer :term2
  attr_writer :term3
  attr_writer :term3
end

class STType end

class STNat < STType
  # Comparison and printing methods
  def ==(type); type.is_a?(STNat) end
  def to_s; "nat" end
end

class STBool < STType
  # Comparison and printing methods
  def ==(type); type.is_a?(STBool) end
  def to_s; "bool" end
end

# Functions have a domain type and a codomain type.
class STFun < STType
  attr_reader :dom
  attr_reader :codom
  
  def initialize(dom, codom)
    unless dom.is_a?(STType) && dom.is_a?(STType)
      throw "Constructing a type out of non-types"
    end
    @dom = dom; @codom = codom
  end

  # Comparison and printing methods
  def ==(type); type.is_a?(STFun) && type.dom == @dom && type.codom == @codom end 
  def to_s; "(" + dom.to_s + ") -> (" + codom.to_s + ")" end
end