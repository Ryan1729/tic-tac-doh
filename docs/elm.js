
(function() {
'use strict';

function F2(fun)
{
  function wrapper(a) { return function(b) { return fun(a,b); }; }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun)
{
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun, a, b)
{
  return fun.arity === 2
    ? fun.func(a, b)
    : fun(a)(b);
}
function A3(fun, a, b, c)
{
  return fun.arity === 3
    ? fun.func(a, b, c)
    : fun(a)(b)(c);
}
function A4(fun, a, b, c, d)
{
  return fun.arity === 4
    ? fun.func(a, b, c, d)
    : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e)
{
  return fun.arity === 5
    ? fun.func(a, b, c, d, e)
    : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f)
{
  return fun.arity === 6
    ? fun.func(a, b, c, d, e, f)
    : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g)
{
  return fun.arity === 7
    ? fun.func(a, b, c, d, e, f, g)
    : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h)
{
  return fun.arity === 8
    ? fun.func(a, b, c, d, e, f, g, h)
    : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i)
{
  return fun.arity === 9
    ? fun.func(a, b, c, d, e, f, g, h, i)
    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

//import Native.Utils //

var _elm_lang$core$Native_Basics = function() {

function div(a, b)
{
	return (a / b) | 0;
}
function rem(a, b)
{
	return a % b;
}
function mod(a, b)
{
	if (b === 0)
	{
		throw new Error('Cannot perform mod 0. Division by zero error.');
	}
	var r = a % b;
	var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

	return m === b ? 0 : m;
}
function logBase(base, n)
{
	return Math.log(n) / Math.log(base);
}
function negate(n)
{
	return -n;
}
function abs(n)
{
	return n < 0 ? -n : n;
}

function min(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) < 0 ? a : b;
}
function max(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) > 0 ? a : b;
}
function clamp(lo, hi, n)
{
	return _elm_lang$core$Native_Utils.cmp(n, lo) < 0
		? lo
		: _elm_lang$core$Native_Utils.cmp(n, hi) > 0
			? hi
			: n;
}

var ord = ['LT', 'EQ', 'GT'];

function compare(x, y)
{
	return { ctor: ord[_elm_lang$core$Native_Utils.cmp(x, y) + 1] };
}

function xor(a, b)
{
	return a !== b;
}
function not(b)
{
	return !b;
}
function isInfinite(n)
{
	return n === Infinity || n === -Infinity;
}

function truncate(n)
{
	return n | 0;
}

function degrees(d)
{
	return d * Math.PI / 180;
}
function turns(t)
{
	return 2 * Math.PI * t;
}
function fromPolar(point)
{
	var r = point._0;
	var t = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
}
function toPolar(point)
{
	var x = point._0;
	var y = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
}

return {
	div: F2(div),
	rem: F2(rem),
	mod: F2(mod),

	pi: Math.PI,
	e: Math.E,
	cos: Math.cos,
	sin: Math.sin,
	tan: Math.tan,
	acos: Math.acos,
	asin: Math.asin,
	atan: Math.atan,
	atan2: F2(Math.atan2),

	degrees: degrees,
	turns: turns,
	fromPolar: fromPolar,
	toPolar: toPolar,

	sqrt: Math.sqrt,
	logBase: F2(logBase),
	negate: negate,
	abs: abs,
	min: F2(min),
	max: F2(max),
	clamp: F3(clamp),
	compare: F2(compare),

	xor: F2(xor),
	not: not,

	truncate: truncate,
	ceiling: Math.ceil,
	floor: Math.floor,
	round: Math.round,
	toFloat: function(x) { return x; },
	isNaN: isNaN,
	isInfinite: isInfinite
};

}();
//import //

var _elm_lang$core$Native_Utils = function() {

// COMPARISONS

function eq(x, y)
{
	var stack = [];
	var isEqual = eqHelp(x, y, 0, stack);
	var pair;
	while (isEqual && (pair = stack.pop()))
	{
		isEqual = eqHelp(pair.x, pair.y, 0, stack);
	}
	return isEqual;
}


function eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push({ x: x, y: y });
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object')
	{
		if (typeof x === 'function')
		{
			throw new Error(
				'Trying to use `(==)` on functions. There is no way to know if functions are "the same" in the Elm sense.'
				+ ' Read more about this at http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#=='
				+ ' which describes why it is this way and what the better version will look like.'
			);
		}
		return false;
	}

	if (x === null || y === null)
	{
		return false
	}

	if (x instanceof Date)
	{
		return x.getTime() === y.getTime();
	}

	if (!('ctor' in x))
	{
		for (var key in x)
		{
			if (!eqHelp(x[key], y[key], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	// convert Dicts and Sets to lists
	if (x.ctor === 'RBNode_elm_builtin' || x.ctor === 'RBEmpty_elm_builtin')
	{
		x = _elm_lang$core$Dict$toList(x);
		y = _elm_lang$core$Dict$toList(y);
	}
	if (x.ctor === 'Set_elm_builtin')
	{
		x = _elm_lang$core$Set$toList(x);
		y = _elm_lang$core$Set$toList(y);
	}

	// check if lists are equal without recursion
	if (x.ctor === '::')
	{
		var a = x;
		var b = y;
		while (a.ctor === '::' && b.ctor === '::')
		{
			if (!eqHelp(a._0, b._0, depth + 1, stack))
			{
				return false;
			}
			a = a._1;
			b = b._1;
		}
		return a.ctor === b.ctor;
	}

	// check if Arrays are equal
	if (x.ctor === '_Array')
	{
		var xs = _elm_lang$core$Native_Array.toJSArray(x);
		var ys = _elm_lang$core$Native_Array.toJSArray(y);
		if (xs.length !== ys.length)
		{
			return false;
		}
		for (var i = 0; i < xs.length; i++)
		{
			if (!eqHelp(xs[i], ys[i], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	if (!eqHelp(x.ctor, y.ctor, depth + 1, stack))
	{
		return false;
	}

	for (var key in x)
	{
		if (!eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

var LT = -1, EQ = 0, GT = 1;

function cmp(x, y)
{
	if (typeof x !== 'object')
	{
		return x === y ? EQ : x < y ? LT : GT;
	}

	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? EQ : a < b ? LT : GT;
	}

	if (x.ctor === '::' || x.ctor === '[]')
	{
		while (x.ctor === '::' && y.ctor === '::')
		{
			var ord = cmp(x._0, y._0);
			if (ord !== EQ)
			{
				return ord;
			}
			x = x._1;
			y = y._1;
		}
		return x.ctor === y.ctor ? EQ : x.ctor === '[]' ? LT : GT;
	}

	if (x.ctor.slice(0, 6) === '_Tuple')
	{
		var ord;
		var n = x.ctor.slice(6) - 0;
		var err = 'cannot compare tuples with more than 6 elements.';
		if (n === 0) return EQ;
		if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
		if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
		if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
		if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
		if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
		if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
		if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
		return EQ;
	}

	throw new Error(
		'Comparison error: comparison is only defined on ints, '
		+ 'floats, times, chars, strings, lists of comparable values, '
		+ 'and tuples of comparable values.'
	);
}


// COMMON VALUES

var Tuple0 = {
	ctor: '_Tuple0'
};

function Tuple2(x, y)
{
	return {
		ctor: '_Tuple2',
		_0: x,
		_1: y
	};
}

function chr(c)
{
	return new String(c);
}


// GUID

var count = 0;
function guid(_)
{
	return count++;
}


// RECORDS

function update(oldRecord, updatedFields)
{
	var newRecord = {};
	for (var key in oldRecord)
	{
		var value = (key in updatedFields) ? updatedFields[key] : oldRecord[key];
		newRecord[key] = value;
	}
	return newRecord;
}


//// LIST STUFF ////

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return {
		ctor: '::',
		_0: hd,
		_1: tl
	};
}

function append(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (xs.ctor === '[]')
	{
		return ys;
	}
	var root = Cons(xs._0, Nil);
	var curr = root;
	xs = xs._1;
	while (xs.ctor !== '[]')
	{
		curr._1 = Cons(xs._0, Nil);
		xs = xs._1;
		curr = curr._1;
	}
	curr._1 = ys;
	return root;
}


// CRASHES

function crash(moduleName, region)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function crashCase(moduleName, region, value)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
			+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
			+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function regionToString(region)
{
	if (region.start.line == region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'between lines ' + region.start.line + ' and ' + region.end.line;
}


// TO STRING

function toString(v)
{
	var type = typeof v;
	if (type === 'function')
	{
		var name = v.func ? v.func.name : v.name;
		return '<function' + (name === '' ? '' : ':') + name + '>';
	}

	if (type === 'boolean')
	{
		return v ? 'True' : 'False';
	}

	if (type === 'number')
	{
		return v + '';
	}

	if (v instanceof String)
	{
		return '\'' + addSlashes(v, true) + '\'';
	}

	if (type === 'string')
	{
		return '"' + addSlashes(v, false) + '"';
	}

	if (v === null)
	{
		return 'null';
	}

	if (type === 'object' && 'ctor' in v)
	{
		var ctorStarter = v.ctor.substring(0, 5);

		if (ctorStarter === '_Tupl')
		{
			var output = [];
			for (var k in v)
			{
				if (k === 'ctor') continue;
				output.push(toString(v[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (ctorStarter === '_Task')
		{
			return '<task>'
		}

		if (v.ctor === '_Array')
		{
			var list = _elm_lang$core$Array$toList(v);
			return 'Array.fromList ' + toString(list);
		}

		if (v.ctor === '<decoder>')
		{
			return '<decoder>';
		}

		if (v.ctor === '_Process')
		{
			return '<process:' + v.id + '>';
		}

		if (v.ctor === '::')
		{
			var output = '[' + toString(v._0);
			v = v._1;
			while (v.ctor === '::')
			{
				output += ',' + toString(v._0);
				v = v._1;
			}
			return output + ']';
		}

		if (v.ctor === '[]')
		{
			return '[]';
		}

		if (v.ctor === 'Set_elm_builtin')
		{
			return 'Set.fromList ' + toString(_elm_lang$core$Set$toList(v));
		}

		if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin')
		{
			return 'Dict.fromList ' + toString(_elm_lang$core$Dict$toList(v));
		}

		var output = '';
		for (var i in v)
		{
			if (i === 'ctor') continue;
			var str = toString(v[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return v.ctor + output;
	}

	if (type === 'object')
	{
		if (v instanceof Date)
		{
			return '<' + v.toString() + '>';
		}

		if (v.elm_web_socket)
		{
			return '<websocket>';
		}

		var output = [];
		for (var k in v)
		{
			output.push(k + ' = ' + toString(v[k]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return '<internal structure>';
}

function addSlashes(str, isChar)
{
	var s = str.replace(/\\/g, '\\\\')
			  .replace(/\n/g, '\\n')
			  .replace(/\t/g, '\\t')
			  .replace(/\r/g, '\\r')
			  .replace(/\v/g, '\\v')
			  .replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}


return {
	eq: eq,
	cmp: cmp,
	Tuple0: Tuple0,
	Tuple2: Tuple2,
	chr: chr,
	update: update,
	guid: guid,

	append: F2(append),

	crash: crash,
	crashCase: crashCase,

	toString: toString
};

}();
var _elm_lang$core$Basics$uncurry = F2(
	function (f, _p0) {
		var _p1 = _p0;
		return A2(f, _p1._0, _p1._1);
	});
var _elm_lang$core$Basics$curry = F3(
	function (f, a, b) {
		return f(
			{ctor: '_Tuple2', _0: a, _1: b});
	});
var _elm_lang$core$Basics$flip = F3(
	function (f, b, a) {
		return A2(f, a, b);
	});
var _elm_lang$core$Basics$snd = function (_p2) {
	var _p3 = _p2;
	return _p3._1;
};
var _elm_lang$core$Basics$fst = function (_p4) {
	var _p5 = _p4;
	return _p5._0;
};
var _elm_lang$core$Basics$always = F2(
	function (a, _p6) {
		return a;
	});
var _elm_lang$core$Basics$identity = function (x) {
	return x;
};
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<|'] = F2(
	function (f, x) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['|>'] = F2(
	function (x, f) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>>'] = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<<'] = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['++'] = _elm_lang$core$Native_Utils.append;
var _elm_lang$core$Basics$toString = _elm_lang$core$Native_Utils.toString;
var _elm_lang$core$Basics$isInfinite = _elm_lang$core$Native_Basics.isInfinite;
var _elm_lang$core$Basics$isNaN = _elm_lang$core$Native_Basics.isNaN;
var _elm_lang$core$Basics$toFloat = _elm_lang$core$Native_Basics.toFloat;
var _elm_lang$core$Basics$ceiling = _elm_lang$core$Native_Basics.ceiling;
var _elm_lang$core$Basics$floor = _elm_lang$core$Native_Basics.floor;
var _elm_lang$core$Basics$truncate = _elm_lang$core$Native_Basics.truncate;
var _elm_lang$core$Basics$round = _elm_lang$core$Native_Basics.round;
var _elm_lang$core$Basics$not = _elm_lang$core$Native_Basics.not;
var _elm_lang$core$Basics$xor = _elm_lang$core$Native_Basics.xor;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['||'] = _elm_lang$core$Native_Basics.or;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['&&'] = _elm_lang$core$Native_Basics.and;
var _elm_lang$core$Basics$max = _elm_lang$core$Native_Basics.max;
var _elm_lang$core$Basics$min = _elm_lang$core$Native_Basics.min;
var _elm_lang$core$Basics$compare = _elm_lang$core$Native_Basics.compare;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>='] = _elm_lang$core$Native_Basics.ge;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<='] = _elm_lang$core$Native_Basics.le;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>'] = _elm_lang$core$Native_Basics.gt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<'] = _elm_lang$core$Native_Basics.lt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/='] = _elm_lang$core$Native_Basics.neq;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['=='] = _elm_lang$core$Native_Basics.eq;
var _elm_lang$core$Basics$e = _elm_lang$core$Native_Basics.e;
var _elm_lang$core$Basics$pi = _elm_lang$core$Native_Basics.pi;
var _elm_lang$core$Basics$clamp = _elm_lang$core$Native_Basics.clamp;
var _elm_lang$core$Basics$logBase = _elm_lang$core$Native_Basics.logBase;
var _elm_lang$core$Basics$abs = _elm_lang$core$Native_Basics.abs;
var _elm_lang$core$Basics$negate = _elm_lang$core$Native_Basics.negate;
var _elm_lang$core$Basics$sqrt = _elm_lang$core$Native_Basics.sqrt;
var _elm_lang$core$Basics$atan2 = _elm_lang$core$Native_Basics.atan2;
var _elm_lang$core$Basics$atan = _elm_lang$core$Native_Basics.atan;
var _elm_lang$core$Basics$asin = _elm_lang$core$Native_Basics.asin;
var _elm_lang$core$Basics$acos = _elm_lang$core$Native_Basics.acos;
var _elm_lang$core$Basics$tan = _elm_lang$core$Native_Basics.tan;
var _elm_lang$core$Basics$sin = _elm_lang$core$Native_Basics.sin;
var _elm_lang$core$Basics$cos = _elm_lang$core$Native_Basics.cos;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['^'] = _elm_lang$core$Native_Basics.exp;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['%'] = _elm_lang$core$Native_Basics.mod;
var _elm_lang$core$Basics$rem = _elm_lang$core$Native_Basics.rem;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['//'] = _elm_lang$core$Native_Basics.div;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/'] = _elm_lang$core$Native_Basics.floatDiv;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['*'] = _elm_lang$core$Native_Basics.mul;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['-'] = _elm_lang$core$Native_Basics.sub;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['+'] = _elm_lang$core$Native_Basics.add;
var _elm_lang$core$Basics$toPolar = _elm_lang$core$Native_Basics.toPolar;
var _elm_lang$core$Basics$fromPolar = _elm_lang$core$Native_Basics.fromPolar;
var _elm_lang$core$Basics$turns = _elm_lang$core$Native_Basics.turns;
var _elm_lang$core$Basics$degrees = _elm_lang$core$Native_Basics.degrees;
var _elm_lang$core$Basics$radians = function (t) {
	return t;
};
var _elm_lang$core$Basics$GT = {ctor: 'GT'};
var _elm_lang$core$Basics$EQ = {ctor: 'EQ'};
var _elm_lang$core$Basics$LT = {ctor: 'LT'};
var _elm_lang$core$Basics$Never = function (a) {
	return {ctor: 'Never', _0: a};
};

//import Native.Utils //

var _elm_lang$core$Native_Debug = function() {

function log(tag, value)
{
	var msg = tag + ': ' + _elm_lang$core$Native_Utils.toString(value);
	var process = process || {};
	if (process.stdout)
	{
		process.stdout.write(msg);
	}
	else
	{
		console.log(msg);
	}
	return value;
}

function crash(message)
{
	throw new Error(message);
}

return {
	crash: crash,
	log: F2(log)
};

}();
var _elm_lang$core$Debug$crash = _elm_lang$core$Native_Debug.crash;
var _elm_lang$core$Debug$log = _elm_lang$core$Native_Debug.log;

var _elm_lang$core$Maybe$withDefault = F2(
	function ($default, maybe) {
		var _p0 = maybe;
		if (_p0.ctor === 'Just') {
			return _p0._0;
		} else {
			return $default;
		}
	});
var _elm_lang$core$Maybe$Nothing = {ctor: 'Nothing'};
var _elm_lang$core$Maybe$oneOf = function (maybes) {
	oneOf:
	while (true) {
		var _p1 = maybes;
		if (_p1.ctor === '[]') {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			var _p3 = _p1._0;
			var _p2 = _p3;
			if (_p2.ctor === 'Nothing') {
				var _v3 = _p1._1;
				maybes = _v3;
				continue oneOf;
			} else {
				return _p3;
			}
		}
	}
};
var _elm_lang$core$Maybe$andThen = F2(
	function (maybeValue, callback) {
		var _p4 = maybeValue;
		if (_p4.ctor === 'Just') {
			return callback(_p4._0);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$Just = function (a) {
	return {ctor: 'Just', _0: a};
};
var _elm_lang$core$Maybe$map = F2(
	function (f, maybe) {
		var _p5 = maybe;
		if (_p5.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				f(_p5._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		var _p6 = {ctor: '_Tuple2', _0: ma, _1: mb};
		if (((_p6.ctor === '_Tuple2') && (_p6._0.ctor === 'Just')) && (_p6._1.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A2(func, _p6._0._0, _p6._1._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map3 = F4(
	function (func, ma, mb, mc) {
		var _p7 = {ctor: '_Tuple3', _0: ma, _1: mb, _2: mc};
		if ((((_p7.ctor === '_Tuple3') && (_p7._0.ctor === 'Just')) && (_p7._1.ctor === 'Just')) && (_p7._2.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A3(func, _p7._0._0, _p7._1._0, _p7._2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map4 = F5(
	function (func, ma, mb, mc, md) {
		var _p8 = {ctor: '_Tuple4', _0: ma, _1: mb, _2: mc, _3: md};
		if (((((_p8.ctor === '_Tuple4') && (_p8._0.ctor === 'Just')) && (_p8._1.ctor === 'Just')) && (_p8._2.ctor === 'Just')) && (_p8._3.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A4(func, _p8._0._0, _p8._1._0, _p8._2._0, _p8._3._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map5 = F6(
	function (func, ma, mb, mc, md, me) {
		var _p9 = {ctor: '_Tuple5', _0: ma, _1: mb, _2: mc, _3: md, _4: me};
		if ((((((_p9.ctor === '_Tuple5') && (_p9._0.ctor === 'Just')) && (_p9._1.ctor === 'Just')) && (_p9._2.ctor === 'Just')) && (_p9._3.ctor === 'Just')) && (_p9._4.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A5(func, _p9._0._0, _p9._1._0, _p9._2._0, _p9._3._0, _p9._4._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});

//import Native.Utils //

var _elm_lang$core$Native_List = function() {

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return { ctor: '::', _0: hd, _1: tl };
}

function fromArray(arr)
{
	var out = Nil;
	for (var i = arr.length; i--; )
	{
		out = Cons(arr[i], out);
	}
	return out;
}

function toArray(xs)
{
	var out = [];
	while (xs.ctor !== '[]')
	{
		out.push(xs._0);
		xs = xs._1;
	}
	return out;
}


function range(lo, hi)
{
	var list = Nil;
	if (lo <= hi)
	{
		do
		{
			list = Cons(hi, list);
		}
		while (hi-- > lo);
	}
	return list;
}

function foldr(f, b, xs)
{
	var arr = toArray(xs);
	var acc = b;
	for (var i = arr.length; i--; )
	{
		acc = A2(f, arr[i], acc);
	}
	return acc;
}

function map2(f, xs, ys)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]')
	{
		arr.push(A2(f, xs._0, ys._0));
		xs = xs._1;
		ys = ys._1;
	}
	return fromArray(arr);
}

function map3(f, xs, ys, zs)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
	{
		arr.push(A3(f, xs._0, ys._0, zs._0));
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map4(f, ws, xs, ys, zs)
{
	var arr = [];
	while (   ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map5(f, vs, ws, xs, ys, zs)
{
	var arr = [];
	while (   vs.ctor !== '[]'
		   && ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
		vs = vs._1;
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function sortBy(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		return _elm_lang$core$Native_Utils.cmp(f(a), f(b));
	}));
}

function sortWith(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		var ord = f(a)(b).ctor;
		return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
	}));
}

return {
	Nil: Nil,
	Cons: Cons,
	cons: F2(Cons),
	toArray: toArray,
	fromArray: fromArray,
	range: range,

	foldr: F3(foldr),

	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	sortBy: F2(sortBy),
	sortWith: F2(sortWith)
};

}();
var _elm_lang$core$List$sortWith = _elm_lang$core$Native_List.sortWith;
var _elm_lang$core$List$sortBy = _elm_lang$core$Native_List.sortBy;
var _elm_lang$core$List$sort = function (xs) {
	return A2(_elm_lang$core$List$sortBy, _elm_lang$core$Basics$identity, xs);
};
var _elm_lang$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return list;
			} else {
				var _p0 = list;
				if (_p0.ctor === '[]') {
					return list;
				} else {
					var _v1 = n - 1,
						_v2 = _p0._1;
					n = _v1;
					list = _v2;
					continue drop;
				}
			}
		}
	});
var _elm_lang$core$List$map5 = _elm_lang$core$Native_List.map5;
var _elm_lang$core$List$map4 = _elm_lang$core$Native_List.map4;
var _elm_lang$core$List$map3 = _elm_lang$core$Native_List.map3;
var _elm_lang$core$List$map2 = _elm_lang$core$Native_List.map2;
var _elm_lang$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			var _p1 = list;
			if (_p1.ctor === '[]') {
				return false;
			} else {
				if (isOkay(_p1._0)) {
					return true;
				} else {
					var _v4 = isOkay,
						_v5 = _p1._1;
					isOkay = _v4;
					list = _v5;
					continue any;
				}
			}
		}
	});
var _elm_lang$core$List$all = F2(
	function (isOkay, list) {
		return _elm_lang$core$Basics$not(
			A2(
				_elm_lang$core$List$any,
				function (_p2) {
					return _elm_lang$core$Basics$not(
						isOkay(_p2));
				},
				list));
	});
var _elm_lang$core$List$foldr = _elm_lang$core$Native_List.foldr;
var _elm_lang$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			var _p3 = list;
			if (_p3.ctor === '[]') {
				return acc;
			} else {
				var _v7 = func,
					_v8 = A2(func, _p3._0, acc),
					_v9 = _p3._1;
				func = _v7;
				acc = _v8;
				list = _v9;
				continue foldl;
			}
		}
	});
var _elm_lang$core$List$length = function (xs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p4, i) {
				return i + 1;
			}),
		0,
		xs);
};
var _elm_lang$core$List$sum = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x + y;
			}),
		0,
		numbers);
};
var _elm_lang$core$List$product = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x * y;
			}),
		1,
		numbers);
};
var _elm_lang$core$List$maximum = function (list) {
	var _p5 = list;
	if (_p5.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$max, _p5._0, _p5._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$minimum = function (list) {
	var _p6 = list;
	if (_p6.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$min, _p6._0, _p6._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$map2,
			f,
			_elm_lang$core$Native_List.range(
				0,
				_elm_lang$core$List$length(xs) - 1),
			xs);
	});
var _elm_lang$core$List$member = F2(
	function (x, xs) {
		return A2(
			_elm_lang$core$List$any,
			function (a) {
				return _elm_lang$core$Native_Utils.eq(a, x);
			},
			xs);
	});
var _elm_lang$core$List$isEmpty = function (xs) {
	var _p7 = xs;
	if (_p7.ctor === '[]') {
		return true;
	} else {
		return false;
	}
};
var _elm_lang$core$List$tail = function (list) {
	var _p8 = list;
	if (_p8.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p8._1);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$head = function (list) {
	var _p9 = list;
	if (_p9.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p9._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List_ops = _elm_lang$core$List_ops || {};
_elm_lang$core$List_ops['::'] = _elm_lang$core$Native_List.cons;
var _elm_lang$core$List$map = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						_elm_lang$core$List_ops['::'],
						f(x),
						acc);
				}),
			_elm_lang$core$Native_List.fromArray(
				[]),
			xs);
	});
var _elm_lang$core$List$filter = F2(
	function (pred, xs) {
		var conditionalCons = F2(
			function (x, xs$) {
				return pred(x) ? A2(_elm_lang$core$List_ops['::'], x, xs$) : xs$;
			});
		return A3(
			_elm_lang$core$List$foldr,
			conditionalCons,
			_elm_lang$core$Native_List.fromArray(
				[]),
			xs);
	});
var _elm_lang$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _p10 = f(mx);
		if (_p10.ctor === 'Just') {
			return A2(_elm_lang$core$List_ops['::'], _p10._0, xs);
		} else {
			return xs;
		}
	});
var _elm_lang$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			_elm_lang$core$List$maybeCons(f),
			_elm_lang$core$Native_List.fromArray(
				[]),
			xs);
	});
var _elm_lang$core$List$reverse = function (list) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return A2(_elm_lang$core$List_ops['::'], x, y);
			}),
		_elm_lang$core$Native_List.fromArray(
			[]),
		list);
};
var _elm_lang$core$List$scanl = F3(
	function (f, b, xs) {
		var scan1 = F2(
			function (x, accAcc) {
				var _p11 = accAcc;
				if (_p11.ctor === '::') {
					return A2(
						_elm_lang$core$List_ops['::'],
						A2(f, x, _p11._0),
						accAcc);
				} else {
					return _elm_lang$core$Native_List.fromArray(
						[]);
				}
			});
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$foldl,
				scan1,
				_elm_lang$core$Native_List.fromArray(
					[b]),
				xs));
	});
var _elm_lang$core$List$append = F2(
	function (xs, ys) {
		var _p12 = ys;
		if (_p12.ctor === '[]') {
			return xs;
		} else {
			return A3(
				_elm_lang$core$List$foldr,
				F2(
					function (x, y) {
						return A2(_elm_lang$core$List_ops['::'], x, y);
					}),
				ys,
				xs);
		}
	});
var _elm_lang$core$List$concat = function (lists) {
	return A3(
		_elm_lang$core$List$foldr,
		_elm_lang$core$List$append,
		_elm_lang$core$Native_List.fromArray(
			[]),
		lists);
};
var _elm_lang$core$List$concatMap = F2(
	function (f, list) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$map, f, list));
	});
var _elm_lang$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _p13) {
				var _p14 = _p13;
				var _p16 = _p14._0;
				var _p15 = _p14._1;
				return pred(x) ? {
					ctor: '_Tuple2',
					_0: A2(_elm_lang$core$List_ops['::'], x, _p16),
					_1: _p15
				} : {
					ctor: '_Tuple2',
					_0: _p16,
					_1: A2(_elm_lang$core$List_ops['::'], x, _p15)
				};
			});
		return A3(
			_elm_lang$core$List$foldr,
			step,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Native_List.fromArray(
					[]),
				_1: _elm_lang$core$Native_List.fromArray(
					[])
			},
			list);
	});
var _elm_lang$core$List$unzip = function (pairs) {
	var step = F2(
		function (_p18, _p17) {
			var _p19 = _p18;
			var _p20 = _p17;
			return {
				ctor: '_Tuple2',
				_0: A2(_elm_lang$core$List_ops['::'], _p19._0, _p20._0),
				_1: A2(_elm_lang$core$List_ops['::'], _p19._1, _p20._1)
			};
		});
	return A3(
		_elm_lang$core$List$foldr,
		step,
		{
			ctor: '_Tuple2',
			_0: _elm_lang$core$Native_List.fromArray(
				[]),
			_1: _elm_lang$core$Native_List.fromArray(
				[])
		},
		pairs);
};
var _elm_lang$core$List$intersperse = F2(
	function (sep, xs) {
		var _p21 = xs;
		if (_p21.ctor === '[]') {
			return _elm_lang$core$Native_List.fromArray(
				[]);
		} else {
			var step = F2(
				function (x, rest) {
					return A2(
						_elm_lang$core$List_ops['::'],
						sep,
						A2(_elm_lang$core$List_ops['::'], x, rest));
				});
			var spersed = A3(
				_elm_lang$core$List$foldr,
				step,
				_elm_lang$core$Native_List.fromArray(
					[]),
				_p21._1);
			return A2(_elm_lang$core$List_ops['::'], _p21._0, spersed);
		}
	});
var _elm_lang$core$List$takeReverse = F3(
	function (n, list, taken) {
		takeReverse:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return taken;
			} else {
				var _p22 = list;
				if (_p22.ctor === '[]') {
					return taken;
				} else {
					var _v23 = n - 1,
						_v24 = _p22._1,
						_v25 = A2(_elm_lang$core$List_ops['::'], _p22._0, taken);
					n = _v23;
					list = _v24;
					taken = _v25;
					continue takeReverse;
				}
			}
		}
	});
var _elm_lang$core$List$takeTailRec = F2(
	function (n, list) {
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$takeReverse,
				n,
				list,
				_elm_lang$core$Native_List.fromArray(
					[])));
	});
var _elm_lang$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
			return _elm_lang$core$Native_List.fromArray(
				[]);
		} else {
			var _p23 = {ctor: '_Tuple2', _0: n, _1: list};
			_v26_5:
			do {
				_v26_1:
				do {
					if (_p23.ctor === '_Tuple2') {
						if (_p23._1.ctor === '[]') {
							return list;
						} else {
							if (_p23._1._1.ctor === '::') {
								switch (_p23._0) {
									case 1:
										break _v26_1;
									case 2:
										return _elm_lang$core$Native_List.fromArray(
											[_p23._1._0, _p23._1._1._0]);
									case 3:
										if (_p23._1._1._1.ctor === '::') {
											return _elm_lang$core$Native_List.fromArray(
												[_p23._1._0, _p23._1._1._0, _p23._1._1._1._0]);
										} else {
											break _v26_5;
										}
									default:
										if ((_p23._1._1._1.ctor === '::') && (_p23._1._1._1._1.ctor === '::')) {
											var _p28 = _p23._1._1._1._0;
											var _p27 = _p23._1._1._0;
											var _p26 = _p23._1._0;
											var _p25 = _p23._1._1._1._1._0;
											var _p24 = _p23._1._1._1._1._1;
											return (_elm_lang$core$Native_Utils.cmp(ctr, 1000) > 0) ? A2(
												_elm_lang$core$List_ops['::'],
												_p26,
												A2(
													_elm_lang$core$List_ops['::'],
													_p27,
													A2(
														_elm_lang$core$List_ops['::'],
														_p28,
														A2(
															_elm_lang$core$List_ops['::'],
															_p25,
															A2(_elm_lang$core$List$takeTailRec, n - 4, _p24))))) : A2(
												_elm_lang$core$List_ops['::'],
												_p26,
												A2(
													_elm_lang$core$List_ops['::'],
													_p27,
													A2(
														_elm_lang$core$List_ops['::'],
														_p28,
														A2(
															_elm_lang$core$List_ops['::'],
															_p25,
															A3(_elm_lang$core$List$takeFast, ctr + 1, n - 4, _p24)))));
										} else {
											break _v26_5;
										}
								}
							} else {
								if (_p23._0 === 1) {
									break _v26_1;
								} else {
									break _v26_5;
								}
							}
						}
					} else {
						break _v26_5;
					}
				} while(false);
				return _elm_lang$core$Native_List.fromArray(
					[_p23._1._0]);
			} while(false);
			return list;
		}
	});
var _elm_lang$core$List$take = F2(
	function (n, list) {
		return A3(_elm_lang$core$List$takeFast, 0, n, list);
	});
var _elm_lang$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return result;
			} else {
				var _v27 = A2(_elm_lang$core$List_ops['::'], value, result),
					_v28 = n - 1,
					_v29 = value;
				result = _v27;
				n = _v28;
				value = _v29;
				continue repeatHelp;
			}
		}
	});
var _elm_lang$core$List$repeat = F2(
	function (n, value) {
		return A3(
			_elm_lang$core$List$repeatHelp,
			_elm_lang$core$Native_List.fromArray(
				[]),
			n,
			value);
	});

var _elm_lang$core$Result$toMaybe = function (result) {
	var _p0 = result;
	if (_p0.ctor === 'Ok') {
		return _elm_lang$core$Maybe$Just(_p0._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$Result$withDefault = F2(
	function (def, result) {
		var _p1 = result;
		if (_p1.ctor === 'Ok') {
			return _p1._0;
		} else {
			return def;
		}
	});
var _elm_lang$core$Result$Err = function (a) {
	return {ctor: 'Err', _0: a};
};
var _elm_lang$core$Result$andThen = F2(
	function (result, callback) {
		var _p2 = result;
		if (_p2.ctor === 'Ok') {
			return callback(_p2._0);
		} else {
			return _elm_lang$core$Result$Err(_p2._0);
		}
	});
var _elm_lang$core$Result$Ok = function (a) {
	return {ctor: 'Ok', _0: a};
};
var _elm_lang$core$Result$map = F2(
	function (func, ra) {
		var _p3 = ra;
		if (_p3.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(
				func(_p3._0));
		} else {
			return _elm_lang$core$Result$Err(_p3._0);
		}
	});
var _elm_lang$core$Result$map2 = F3(
	function (func, ra, rb) {
		var _p4 = {ctor: '_Tuple2', _0: ra, _1: rb};
		if (_p4._0.ctor === 'Ok') {
			if (_p4._1.ctor === 'Ok') {
				return _elm_lang$core$Result$Ok(
					A2(func, _p4._0._0, _p4._1._0));
			} else {
				return _elm_lang$core$Result$Err(_p4._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p4._0._0);
		}
	});
var _elm_lang$core$Result$map3 = F4(
	function (func, ra, rb, rc) {
		var _p5 = {ctor: '_Tuple3', _0: ra, _1: rb, _2: rc};
		if (_p5._0.ctor === 'Ok') {
			if (_p5._1.ctor === 'Ok') {
				if (_p5._2.ctor === 'Ok') {
					return _elm_lang$core$Result$Ok(
						A3(func, _p5._0._0, _p5._1._0, _p5._2._0));
				} else {
					return _elm_lang$core$Result$Err(_p5._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p5._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p5._0._0);
		}
	});
var _elm_lang$core$Result$map4 = F5(
	function (func, ra, rb, rc, rd) {
		var _p6 = {ctor: '_Tuple4', _0: ra, _1: rb, _2: rc, _3: rd};
		if (_p6._0.ctor === 'Ok') {
			if (_p6._1.ctor === 'Ok') {
				if (_p6._2.ctor === 'Ok') {
					if (_p6._3.ctor === 'Ok') {
						return _elm_lang$core$Result$Ok(
							A4(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0));
					} else {
						return _elm_lang$core$Result$Err(_p6._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p6._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p6._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p6._0._0);
		}
	});
var _elm_lang$core$Result$map5 = F6(
	function (func, ra, rb, rc, rd, re) {
		var _p7 = {ctor: '_Tuple5', _0: ra, _1: rb, _2: rc, _3: rd, _4: re};
		if (_p7._0.ctor === 'Ok') {
			if (_p7._1.ctor === 'Ok') {
				if (_p7._2.ctor === 'Ok') {
					if (_p7._3.ctor === 'Ok') {
						if (_p7._4.ctor === 'Ok') {
							return _elm_lang$core$Result$Ok(
								A5(func, _p7._0._0, _p7._1._0, _p7._2._0, _p7._3._0, _p7._4._0));
						} else {
							return _elm_lang$core$Result$Err(_p7._4._0);
						}
					} else {
						return _elm_lang$core$Result$Err(_p7._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p7._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p7._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p7._0._0);
		}
	});
var _elm_lang$core$Result$formatError = F2(
	function (f, result) {
		var _p8 = result;
		if (_p8.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(_p8._0);
		} else {
			return _elm_lang$core$Result$Err(
				f(_p8._0));
		}
	});
var _elm_lang$core$Result$fromMaybe = F2(
	function (err, maybe) {
		var _p9 = maybe;
		if (_p9.ctor === 'Just') {
			return _elm_lang$core$Result$Ok(_p9._0);
		} else {
			return _elm_lang$core$Result$Err(err);
		}
	});

//import //

var _elm_lang$core$Native_Platform = function() {


// PROGRAMS

function addPublicModule(object, name, main)
{
	var init = main ? makeEmbed(name, main) : mainIsUndefined(name);

	object['worker'] = function worker(flags)
	{
		return init(undefined, flags, false);
	}

	object['embed'] = function embed(domNode, flags)
	{
		return init(domNode, flags, true);
	}

	object['fullscreen'] = function fullscreen(flags)
	{
		return init(document.body, flags, true);
	};
}


// PROGRAM FAIL

function mainIsUndefined(name)
{
	return function(domNode)
	{
		var message = 'Cannot initialize module `' + name +
			'` because it has no `main` value!\nWhat should I show on screen?';
		domNode.innerHTML = errorHtml(message);
		throw new Error(message);
	};
}

function errorHtml(message)
{
	return '<div style="padding-left:1em;">'
		+ '<h2 style="font-weight:normal;"><b>Oops!</b> Something went wrong when starting your Elm program.</h2>'
		+ '<pre style="padding-left:1em;">' + message + '</pre>'
		+ '</div>';
}


// PROGRAM SUCCESS

function makeEmbed(moduleName, main)
{
	return function embed(rootDomNode, flags, withRenderer)
	{
		try
		{
			var program = mainToProgram(moduleName, main);
			if (!withRenderer)
			{
				program.renderer = dummyRenderer;
			}
			return makeEmbedHelp(moduleName, program, rootDomNode, flags);
		}
		catch (e)
		{
			rootDomNode.innerHTML = errorHtml(e.message);
			throw e;
		}
	};
}

function dummyRenderer()
{
	return { update: function() {} };
}


// MAIN TO PROGRAM

function mainToProgram(moduleName, wrappedMain)
{
	var main = wrappedMain.main;

	if (typeof main.init === 'undefined')
	{
		var emptyBag = batch(_elm_lang$core$Native_List.Nil);
		var noChange = _elm_lang$core$Native_Utils.Tuple2(
			_elm_lang$core$Native_Utils.Tuple0,
			emptyBag
		);

		return _elm_lang$virtual_dom$VirtualDom$programWithFlags({
			init: function() { return noChange; },
			view: function() { return main; },
			update: F2(function() { return noChange; }),
			subscriptions: function () { return emptyBag; }
		});
	}

	var flags = wrappedMain.flags;
	var init = flags
		? initWithFlags(moduleName, main.init, flags)
		: initWithoutFlags(moduleName, main.init);

	return _elm_lang$virtual_dom$VirtualDom$programWithFlags({
		init: init,
		view: main.view,
		update: main.update,
		subscriptions: main.subscriptions,
	});
}

function initWithoutFlags(moduleName, realInit)
{
	return function init(flags)
	{
		if (typeof flags !== 'undefined')
		{
			throw new Error(
				'You are giving module `' + moduleName + '` an argument in JavaScript.\n'
				+ 'This module does not take arguments though! You probably need to change the\n'
				+ 'initialization code to something like `Elm.' + moduleName + '.fullscreen()`'
			);
		}
		return realInit();
	};
}

function initWithFlags(moduleName, realInit, flagDecoder)
{
	return function init(flags)
	{
		var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
		if (result.ctor === 'Err')
		{
			throw new Error(
				'You are trying to initialize module `' + moduleName + '` with an unexpected argument.\n'
				+ 'When trying to convert it to a usable Elm value, I run into this problem:\n\n'
				+ result._0
			);
		}
		return realInit(result._0);
	};
}


// SETUP RUNTIME SYSTEM

function makeEmbedHelp(moduleName, program, rootDomNode, flags)
{
	var init = program.init;
	var update = program.update;
	var subscriptions = program.subscriptions;
	var view = program.view;
	var makeRenderer = program.renderer;

	// ambient state
	var managers = {};
	var renderer;

	// init and update state in main process
	var initApp = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
		var results = init(flags);
		var model = results._0;
		renderer = makeRenderer(rootDomNode, enqueue, view(model));
		var cmds = results._1;
		var subs = subscriptions(model);
		dispatchEffects(managers, cmds, subs);
		callback(_elm_lang$core$Native_Scheduler.succeed(model));
	});

	function onMessage(msg, model)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
			var results = A2(update, msg, model);
			model = results._0;
			renderer.update(view(model));
			var cmds = results._1;
			var subs = subscriptions(model);
			dispatchEffects(managers, cmds, subs);
			callback(_elm_lang$core$Native_Scheduler.succeed(model));
		});
	}

	var mainProcess = spawnLoop(initApp, onMessage);

	function enqueue(msg)
	{
		_elm_lang$core$Native_Scheduler.rawSend(mainProcess, msg);
	}

	var ports = setupEffects(managers, enqueue);

	return ports ? { ports: ports } : {};
}


// EFFECT MANAGERS

var effectManagers = {};

function setupEffects(managers, callback)
{
	var ports;

	// setup all necessary effect managers
	for (var key in effectManagers)
	{
		var manager = effectManagers[key];

		if (manager.isForeign)
		{
			ports = ports || {};
			ports[key] = manager.tag === 'cmd'
				? setupOutgoingPort(key)
				: setupIncomingPort(key, callback);
		}

		managers[key] = makeManager(manager, callback);
	}

	return ports;
}

function makeManager(info, callback)
{
	var router = {
		main: callback,
		self: undefined
	};

	var tag = info.tag;
	var onEffects = info.onEffects;
	var onSelfMsg = info.onSelfMsg;

	function onMessage(msg, state)
	{
		if (msg.ctor === 'self')
		{
			return A3(onSelfMsg, router, msg._0, state);
		}

		var fx = msg._0;
		switch (tag)
		{
			case 'cmd':
				return A3(onEffects, router, fx.cmds, state);

			case 'sub':
				return A3(onEffects, router, fx.subs, state);

			case 'fx':
				return A4(onEffects, router, fx.cmds, fx.subs, state);
		}
	}

	var process = spawnLoop(info.init, onMessage);
	router.self = process;
	return process;
}

function sendToApp(router, msg)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		router.main(msg);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sendToSelf(router, msg)
{
	return A2(_elm_lang$core$Native_Scheduler.send, router.self, {
		ctor: 'self',
		_0: msg
	});
}


// HELPER for STATEFUL LOOPS

function spawnLoop(init, onMessage)
{
	var andThen = _elm_lang$core$Native_Scheduler.andThen;

	function loop(state)
	{
		var handleMsg = _elm_lang$core$Native_Scheduler.receive(function(msg) {
			return onMessage(msg, state);
		});
		return A2(andThen, handleMsg, loop);
	}

	var task = A2(andThen, init, loop);

	return _elm_lang$core$Native_Scheduler.rawSpawn(task);
}


// BAGS

function leaf(home)
{
	return function(value)
	{
		return {
			type: 'leaf',
			home: home,
			value: value
		};
	};
}

function batch(list)
{
	return {
		type: 'node',
		branches: list
	};
}

function map(tagger, bag)
{
	return {
		type: 'map',
		tagger: tagger,
		tree: bag
	}
}


// PIPE BAGS INTO EFFECT MANAGERS

function dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	gatherEffects(true, cmdBag, effectsDict, null);
	gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		var fx = home in effectsDict
			? effectsDict[home]
			: {
				cmds: _elm_lang$core$Native_List.Nil,
				subs: _elm_lang$core$Native_List.Nil
			};

		_elm_lang$core$Native_Scheduler.rawSend(managers[home], { ctor: 'fx', _0: fx });
	}
}

function gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.type)
	{
		case 'leaf':
			var home = bag.home;
			var effect = toEffect(isCmd, home, taggers, bag.value);
			effectsDict[home] = insert(isCmd, effect, effectsDict[home]);
			return;

		case 'node':
			var list = bag.branches;
			while (list.ctor !== '[]')
			{
				gatherEffects(isCmd, list._0, effectsDict, taggers);
				list = list._1;
			}
			return;

		case 'map':
			gatherEffects(isCmd, bag.tree, effectsDict, {
				tagger: bag.tagger,
				rest: taggers
			});
			return;
	}
}

function toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		var temp = taggers;
		while (temp)
		{
			x = temp.tagger(x);
			temp = temp.rest;
		}
		return x;
	}

	var map = isCmd
		? effectManagers[home].cmdMap
		: effectManagers[home].subMap;

	return A2(map, applyTaggers, value)
}

function insert(isCmd, newEffect, effects)
{
	effects = effects || {
		cmds: _elm_lang$core$Native_List.Nil,
		subs: _elm_lang$core$Native_List.Nil
	};
	if (isCmd)
	{
		effects.cmds = _elm_lang$core$Native_List.Cons(newEffect, effects.cmds);
		return effects;
	}
	effects.subs = _elm_lang$core$Native_List.Cons(newEffect, effects.subs);
	return effects;
}


// PORTS

function checkPortName(name)
{
	if (name in effectManagers)
	{
		throw new Error('There can only be one port named `' + name + '`, but your program has multiple.');
	}
}


// OUTGOING PORTS

function outgoingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'cmd',
		cmdMap: outgoingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var outgoingPortMap = F2(function cmdMap(tagger, value) {
	return value;
});

function setupOutgoingPort(name)
{
	var subs = [];
	var converter = effectManagers[name].converter;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function onEffects(router, cmdList, state)
	{
		while (cmdList.ctor !== '[]')
		{
			var value = converter(cmdList._0);
			for (var i = 0; i < subs.length; i++)
			{
				subs[i](value);
			}
			cmdList = cmdList._1;
		}
		return init;
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}


// INCOMING PORTS

function incomingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'sub',
		subMap: incomingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var incomingPortMap = F2(function subMap(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});

function setupIncomingPort(name, callback)
{
	var sentBeforeInit = [];
	var subs = _elm_lang$core$Native_List.Nil;
	var converter = effectManagers[name].converter;
	var currentOnEffects = preInitOnEffects;
	var currentSend = preInitSend;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function preInitOnEffects(router, subList, state)
	{
		var postInitResult = postInitOnEffects(router, subList, state);

		for(var i = 0; i < sentBeforeInit.length; i++)
		{
			postInitSend(sentBeforeInit[i]);
		}

		sentBeforeInit = null; // to release objects held in queue
		currentSend = postInitSend;
		currentOnEffects = postInitOnEffects;
		return postInitResult;
	}

	function postInitOnEffects(router, subList, state)
	{
		subs = subList;
		return init;
	}

	function onEffects(router, subList, state)
	{
		return currentOnEffects(router, subList, state);
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function preInitSend(value)
	{
		sentBeforeInit.push(value);
	}

	function postInitSend(incomingValue)
	{
		var result = A2(_elm_lang$core$Json_Decode$decodeValue, converter, incomingValue);
		if (result.ctor === 'Err')
		{
			throw new Error('Trying to send an unexpected type of value through port `' + name + '`:\n' + result._0);
		}

		var value = result._0;
		var temp = subs;
		while (temp.ctor !== '[]')
		{
			callback(temp._0(value));
			temp = temp._1;
		}
	}

	function send(incomingValue)
	{
		currentSend(incomingValue);
	}

	return { send: send };
}

return {
	// routers
	sendToApp: F2(sendToApp),
	sendToSelf: F2(sendToSelf),

	// global setup
	mainToProgram: mainToProgram,
	effectManagers: effectManagers,
	outgoingPort: outgoingPort,
	incomingPort: incomingPort,
	addPublicModule: addPublicModule,

	// effect bags
	leaf: leaf,
	batch: batch,
	map: F2(map)
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Scheduler = function() {

var MAX_STEPS = 10000;


// TASKS

function succeed(value)
{
	return {
		ctor: '_Task_succeed',
		value: value
	};
}

function fail(error)
{
	return {
		ctor: '_Task_fail',
		value: error
	};
}

function nativeBinding(callback)
{
	return {
		ctor: '_Task_nativeBinding',
		callback: callback,
		cancel: null
	};
}

function andThen(task, callback)
{
	return {
		ctor: '_Task_andThen',
		task: task,
		callback: callback
	};
}

function onError(task, callback)
{
	return {
		ctor: '_Task_onError',
		task: task,
		callback: callback
	};
}

function receive(callback)
{
	return {
		ctor: '_Task_receive',
		callback: callback
	};
}


// PROCESSES

function rawSpawn(task)
{
	var process = {
		ctor: '_Process',
		id: _elm_lang$core$Native_Utils.guid(),
		root: task,
		stack: null,
		mailbox: []
	};

	enqueue(process);

	return process;
}

function spawn(task)
{
	return nativeBinding(function(callback) {
		var process = rawSpawn(task);
		callback(succeed(process));
	});
}

function rawSend(process, msg)
{
	process.mailbox.push(msg);
	enqueue(process);
}

function send(process, msg)
{
	return nativeBinding(function(callback) {
		rawSend(process, msg);
		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function kill(process)
{
	return nativeBinding(function(callback) {
		var root = process.root;
		if (root.ctor === '_Task_nativeBinding' && root.cancel)
		{
			root.cancel();
		}

		process.root = null;

		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sleep(time)
{
	return nativeBinding(function(callback) {
		var id = setTimeout(function() {
			callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}


// STEP PROCESSES

function step(numSteps, process)
{
	while (numSteps < MAX_STEPS)
	{
		var ctor = process.root.ctor;

		if (ctor === '_Task_succeed')
		{
			while (process.stack && process.stack.ctor === '_Task_onError')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_fail')
		{
			while (process.stack && process.stack.ctor === '_Task_andThen')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_andThen')
		{
			process.stack = {
				ctor: '_Task_andThen',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_onError')
		{
			process.stack = {
				ctor: '_Task_onError',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_nativeBinding')
		{
			process.root.cancel = process.root.callback(function(newRoot) {
				process.root = newRoot;
				enqueue(process);
			});

			break;
		}

		if (ctor === '_Task_receive')
		{
			var mailbox = process.mailbox;
			if (mailbox.length === 0)
			{
				break;
			}

			process.root = process.root.callback(mailbox.shift());
			++numSteps;
			continue;
		}

		throw new Error(ctor);
	}

	if (numSteps < MAX_STEPS)
	{
		return numSteps + 1;
	}
	enqueue(process);

	return numSteps;
}


// WORK QUEUE

var working = false;
var workQueue = [];

function enqueue(process)
{
	workQueue.push(process);

	if (!working)
	{
		setTimeout(work, 0);
		working = true;
	}
}

function work()
{
	var numSteps = 0;
	var process;
	while (numSteps < MAX_STEPS && (process = workQueue.shift()))
	{
		if (process.root)
		{
			numSteps = step(numSteps, process);
		}
	}
	if (!process)
	{
		working = false;
		return;
	}
	setTimeout(work, 0);
}


return {
	succeed: succeed,
	fail: fail,
	nativeBinding: nativeBinding,
	andThen: F2(andThen),
	onError: F2(onError),
	receive: receive,

	spawn: spawn,
	kill: kill,
	sleep: sleep,
	send: F2(send),

	rawSpawn: rawSpawn,
	rawSend: rawSend
};

}();
var _elm_lang$core$Platform$hack = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Platform$sendToSelf = _elm_lang$core$Native_Platform.sendToSelf;
var _elm_lang$core$Platform$sendToApp = _elm_lang$core$Native_Platform.sendToApp;
var _elm_lang$core$Platform$Program = {ctor: 'Program'};
var _elm_lang$core$Platform$Task = {ctor: 'Task'};
var _elm_lang$core$Platform$ProcessId = {ctor: 'ProcessId'};
var _elm_lang$core$Platform$Router = {ctor: 'Router'};

var _elm_lang$core$Platform_Cmd$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Cmd$none = _elm_lang$core$Platform_Cmd$batch(
	_elm_lang$core$Native_List.fromArray(
		[]));
var _elm_lang$core$Platform_Cmd_ops = _elm_lang$core$Platform_Cmd_ops || {};
_elm_lang$core$Platform_Cmd_ops['!'] = F2(
	function (model, commands) {
		return {
			ctor: '_Tuple2',
			_0: model,
			_1: _elm_lang$core$Platform_Cmd$batch(commands)
		};
	});
var _elm_lang$core$Platform_Cmd$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Cmd$Cmd = {ctor: 'Cmd'};

var _elm_lang$core$Platform_Sub$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Sub$none = _elm_lang$core$Platform_Sub$batch(
	_elm_lang$core$Native_List.fromArray(
		[]));
var _elm_lang$core$Platform_Sub$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Sub$Sub = {ctor: 'Sub'};

//import Native.List //

var _elm_lang$core$Native_Array = function() {

// A RRB-Tree has two distinct data types.
// Leaf -> "height"  is always 0
//         "table"   is an array of elements
// Node -> "height"  is always greater than 0
//         "table"   is an array of child nodes
//         "lengths" is an array of accumulated lengths of the child nodes

// M is the maximal table size. 32 seems fast. E is the allowed increase
// of search steps when concatting to find an index. Lower values will
// decrease balancing, but will increase search steps.
var M = 32;
var E = 2;

// An empty array.
var empty = {
	ctor: '_Array',
	height: 0,
	table: []
};


function get(i, array)
{
	if (i < 0 || i >= length(array))
	{
		throw new Error(
			'Index ' + i + ' is out of range. Check the length of ' +
			'your array first or use getMaybe or getWithDefault.');
	}
	return unsafeGet(i, array);
}


function unsafeGet(i, array)
{
	for (var x = array.height; x > 0; x--)
	{
		var slot = i >> (x * 5);
		while (array.lengths[slot] <= i)
		{
			slot++;
		}
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array = array.table[slot];
	}
	return array.table[i];
}


// Sets the value at the index i. Only the nodes leading to i will get
// copied and updated.
function set(i, item, array)
{
	if (i < 0 || length(array) <= i)
	{
		return array;
	}
	return unsafeSet(i, item, array);
}


function unsafeSet(i, item, array)
{
	array = nodeCopy(array);

	if (array.height === 0)
	{
		array.table[i] = item;
	}
	else
	{
		var slot = getSlot(i, array);
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array.table[slot] = unsafeSet(i, item, array.table[slot]);
	}
	return array;
}


function initialize(len, f)
{
	if (len <= 0)
	{
		return empty;
	}
	var h = Math.floor( Math.log(len) / Math.log(M) );
	return initialize_(f, h, 0, len);
}

function initialize_(f, h, from, to)
{
	if (h === 0)
	{
		var table = new Array((to - from) % (M + 1));
		for (var i = 0; i < table.length; i++)
		{
		  table[i] = f(from + i);
		}
		return {
			ctor: '_Array',
			height: 0,
			table: table
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

function fromList(list)
{
	if (list.ctor === '[]')
	{
		return empty;
	}

	// Allocate M sized blocks (table) and write list elements to it.
	var table = new Array(M);
	var nodes = [];
	var i = 0;

	while (list.ctor !== '[]')
	{
		table[i] = list._0;
		list = list._1;
		i++;

		// table is full, so we can push a leaf containing it into the
		// next node.
		if (i === M)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table
			};
			fromListPush(leaf, nodes);
			table = new Array(M);
			i = 0;
		}
	}

	// Maybe there is something left on the table.
	if (i > 0)
	{
		var leaf = {
			ctor: '_Array',
			height: 0,
			table: table.splice(0, i)
		};
		fromListPush(leaf, nodes);
	}

	// Go through all of the nodes and eventually push them into higher nodes.
	for (var h = 0; h < nodes.length - 1; h++)
	{
		if (nodes[h].table.length > 0)
		{
			fromListPush(nodes[h], nodes);
		}
	}

	var head = nodes[nodes.length - 1];
	if (head.height > 0 && head.table.length === 1)
	{
		return head.table[0];
	}
	else
	{
		return head;
	}
}

// Push a node into a higher node as a child.
function fromListPush(toPush, nodes)
{
	var h = toPush.height;

	// Maybe the node on this height does not exist.
	if (nodes.length === h)
	{
		var node = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
		nodes.push(node);
	}

	nodes[h].table.push(toPush);
	var len = length(toPush);
	if (nodes[h].lengths.length > 0)
	{
		len += nodes[h].lengths[nodes[h].lengths.length - 1];
	}
	nodes[h].lengths.push(len);

	if (nodes[h].table.length === M)
	{
		fromListPush(nodes[h], nodes);
		nodes[h] = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
	}
}

// Pushes an item via push_ to the bottom right of a tree.
function push(item, a)
{
	var pushed = push_(item, a);
	if (pushed !== null)
	{
		return pushed;
	}

	var newTree = create(item, a.height);
	return siblise(a, newTree);
}

// Recursively tries to push an item to the bottom-right most
// tree possible. If there is no space left for the item,
// null will be returned.
function push_(item, a)
{
	// Handle resursion stop at leaf level.
	if (a.height === 0)
	{
		if (a.table.length < M)
		{
			var newA = {
				ctor: '_Array',
				height: 0,
				table: a.table.slice()
			};
			newA.table.push(item);
			return newA;
		}
		else
		{
		  return null;
		}
	}

	// Recursively push
	var pushed = push_(item, botRight(a));

	// There was space in the bottom right tree, so the slot will
	// be updated.
	if (pushed !== null)
	{
		var newA = nodeCopy(a);
		newA.table[newA.table.length - 1] = pushed;
		newA.lengths[newA.lengths.length - 1]++;
		return newA;
	}

	// When there was no space left, check if there is space left
	// for a new slot with a tree which contains only the item
	// at the bottom.
	if (a.table.length < M)
	{
		var newSlot = create(item, a.height - 1);
		var newA = nodeCopy(a);
		newA.table.push(newSlot);
		newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
		return newA;
	}
	else
	{
		return null;
	}
}

// Converts an array into a list of elements.
function toList(a)
{
	return toList_(_elm_lang$core$Native_List.Nil, a);
}

function toList_(list, a)
{
	for (var i = a.table.length - 1; i >= 0; i--)
	{
		list =
			a.height === 0
				? _elm_lang$core$Native_List.Cons(a.table[i], list)
				: toList_(list, a.table[i]);
	}
	return list;
}

// Maps a function over the elements of an array.
function map(f, a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? f(a.table[i])
				: map(f, a.table[i]);
	}
	return newA;
}

// Maps a function over the elements with their index as first argument.
function indexedMap(f, a)
{
	return indexedMap_(f, a, 0);
}

function indexedMap_(f, a, from)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? A2(f, from + i, a.table[i])
				: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
	}
	return newA;
}

function foldl(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = foldl(f, b, a.table[i]);
		}
	}
	return b;
}

function foldr(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = a.table.length; i--; )
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = a.table.length; i--; )
		{
			b = foldr(f, b, a.table[i]);
		}
	}
	return b;
}

// TODO: currently, it slices the right, then the left. This can be
// optimized.
function slice(from, to, a)
{
	if (from < 0)
	{
		from += length(a);
	}
	if (to < 0)
	{
		to += length(a);
	}
	return sliceLeft(from, sliceRight(to, a));
}

function sliceRight(to, a)
{
	if (to === length(a))
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(0, to);
		return newA;
	}

	// Slice the right recursively.
	var right = getSlot(to, a);
	var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (right === 0)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(0, right),
		lengths: a.lengths.slice(0, right)
	};
	if (sliced.table.length > 0)
	{
		newA.table[right] = sliced;
		newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
	}
	return newA;
}

function sliceLeft(from, a)
{
	if (from === 0)
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(from, a.table.length + 1);
		return newA;
	}

	// Slice the left recursively.
	var left = getSlot(from, a);
	var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (left === a.table.length - 1)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(left, a.table.length + 1),
		lengths: new Array(a.table.length - left)
	};
	newA.table[0] = sliced;
	var len = 0;
	for (var i = 0; i < newA.table.length; i++)
	{
		len += length(newA.table[i]);
		newA.lengths[i] = len;
	}

	return newA;
}

// Appends two trees.
function append(a,b)
{
	if (a.table.length === 0)
	{
		return b;
	}
	if (b.table.length === 0)
	{
		return a;
	}

	var c = append_(a, b);

	// Check if both nodes can be crunshed together.
	if (c[0].table.length + c[1].table.length <= M)
	{
		if (c[0].table.length === 0)
		{
			return c[1];
		}
		if (c[1].table.length === 0)
		{
			return c[0];
		}

		// Adjust .table and .lengths
		c[0].table = c[0].table.concat(c[1].table);
		if (c[0].height > 0)
		{
			var len = length(c[0]);
			for (var i = 0; i < c[1].lengths.length; i++)
			{
				c[1].lengths[i] += len;
			}
			c[0].lengths = c[0].lengths.concat(c[1].lengths);
		}

		return c[0];
	}

	if (c[0].height > 0)
	{
		var toRemove = calcToRemove(a, b);
		if (toRemove > E)
		{
			c = shuffle(c[0], c[1], toRemove);
		}
	}

	return siblise(c[0], c[1]);
}

// Returns an array of two nodes; right and left. One node _may_ be empty.
function append_(a, b)
{
	if (a.height === 0 && b.height === 0)
	{
		return [a, b];
	}

	if (a.height !== 1 || b.height !== 1)
	{
		if (a.height === b.height)
		{
			a = nodeCopy(a);
			b = nodeCopy(b);
			var appended = append_(botRight(a), botLeft(b));

			insertRight(a, appended[1]);
			insertLeft(b, appended[0]);
		}
		else if (a.height > b.height)
		{
			a = nodeCopy(a);
			var appended = append_(botRight(a), b);

			insertRight(a, appended[0]);
			b = parentise(appended[1], appended[1].height + 1);
		}
		else
		{
			b = nodeCopy(b);
			var appended = append_(a, botLeft(b));

			var left = appended[0].table.length === 0 ? 0 : 1;
			var right = left === 0 ? 1 : 0;
			insertLeft(b, appended[left]);
			a = parentise(appended[right], appended[right].height + 1);
		}
	}

	// Check if balancing is needed and return based on that.
	if (a.table.length === 0 || b.table.length === 0)
	{
		return [a, b];
	}

	var toRemove = calcToRemove(a, b);
	if (toRemove <= E)
	{
		return [a, b];
	}
	return shuffle(a, b, toRemove);
}

// Helperfunctions for append_. Replaces a child node at the side of the parent.
function insertRight(parent, node)
{
	var index = parent.table.length - 1;
	parent.table[index] = node;
	parent.lengths[index] = length(node);
	parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
}

function insertLeft(parent, node)
{
	if (node.table.length > 0)
	{
		parent.table[0] = node;
		parent.lengths[0] = length(node);

		var len = length(parent.table[0]);
		for (var i = 1; i < parent.lengths.length; i++)
		{
			len += length(parent.table[i]);
			parent.lengths[i] = len;
		}
	}
	else
	{
		parent.table.shift();
		for (var i = 1; i < parent.lengths.length; i++)
		{
			parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
		}
		parent.lengths.shift();
	}
}

// Returns the extra search steps for E. Refer to the paper.
function calcToRemove(a, b)
{
	var subLengths = 0;
	for (var i = 0; i < a.table.length; i++)
	{
		subLengths += a.table[i].table.length;
	}
	for (var i = 0; i < b.table.length; i++)
	{
		subLengths += b.table[i].table.length;
	}

	var toRemove = a.table.length + b.table.length;
	return toRemove - (Math.floor((subLengths - 1) / M) + 1);
}

// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
function get2(a, b, index)
{
	return index < a.length
		? a[index]
		: b[index - a.length];
}

function set2(a, b, index, value)
{
	if (index < a.length)
	{
		a[index] = value;
	}
	else
	{
		b[index - a.length] = value;
	}
}

function saveSlot(a, b, index, slot)
{
	set2(a.table, b.table, index, slot);

	var l = (index === 0 || index === a.lengths.length)
		? 0
		: get2(a.lengths, a.lengths, index - 1);

	set2(a.lengths, b.lengths, index, l + length(slot));
}

// Creates a node or leaf with a given length at their arrays for perfomance.
// Is only used by shuffle.
function createNode(h, length)
{
	if (length < 0)
	{
		length = 0;
	}
	var a = {
		ctor: '_Array',
		height: h,
		table: new Array(length)
	};
	if (h > 0)
	{
		a.lengths = new Array(length);
	}
	return a;
}

// Returns an array of two balanced nodes.
function shuffle(a, b, toRemove)
{
	var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
	var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

	// Skip the slots with size M. More precise: copy the slot references
	// to the new node
	var read = 0;
	while (get2(a.table, b.table, read).table.length % M === 0)
	{
		set2(newA.table, newB.table, read, get2(a.table, b.table, read));
		set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
		read++;
	}

	// Pulling items from left to right, caching in a slot before writing
	// it into the new nodes.
	var write = read;
	var slot = new createNode(a.height - 1, 0);
	var from = 0;

	// If the current slot is still containing data, then there will be at
	// least one more write, so we do not break this loop yet.
	while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
	{
		// Find out the max possible items for copying.
		var source = get2(a.table, b.table, read);
		var to = Math.min(M - slot.table.length, source.table.length);

		// Copy and adjust size table.
		slot.table = slot.table.concat(source.table.slice(from, to));
		if (slot.height > 0)
		{
			var len = slot.lengths.length;
			for (var i = len; i < len + to - from; i++)
			{
				slot.lengths[i] = length(slot.table[i]);
				slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
			}
		}

		from += to;

		// Only proceed to next slots[i] if the current one was
		// fully copied.
		if (source.table.length <= to)
		{
			read++; from = 0;
		}

		// Only create a new slot if the current one is filled up.
		if (slot.table.length === M)
		{
			saveSlot(newA, newB, write, slot);
			slot = createNode(a.height - 1, 0);
			write++;
		}
	}

	// Cleanup after the loop. Copy the last slot into the new nodes.
	if (slot.table.length > 0)
	{
		saveSlot(newA, newB, write, slot);
		write++;
	}

	// Shift the untouched slots to the left
	while (read < a.table.length + b.table.length )
	{
		saveSlot(newA, newB, write, get2(a.table, b.table, read));
		read++;
		write++;
	}

	return [newA, newB];
}

// Navigation functions
function botRight(a)
{
	return a.table[a.table.length - 1];
}
function botLeft(a)
{
	return a.table[0];
}

// Copies a node for updating. Note that you should not use this if
// only updating only one of "table" or "lengths" for performance reasons.
function nodeCopy(a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice()
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths.slice();
	}
	return newA;
}

// Returns how many items are in the tree.
function length(array)
{
	if (array.height === 0)
	{
		return array.table.length;
	}
	else
	{
		return array.lengths[array.lengths.length - 1];
	}
}

// Calculates in which slot of "table" the item probably is, then
// find the exact slot via forward searching in  "lengths". Returns the index.
function getSlot(i, a)
{
	var slot = i >> (5 * a.height);
	while (a.lengths[slot] <= i)
	{
		slot++;
	}
	return slot;
}

// Recursively creates a tree with a given height containing
// only the given item.
function create(item, h)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: [item]
		};
	}
	return {
		ctor: '_Array',
		height: h,
		table: [create(item, h - 1)],
		lengths: [1]
	};
}

// Recursively creates a tree that contains the given tree.
function parentise(tree, h)
{
	if (h === tree.height)
	{
		return tree;
	}

	return {
		ctor: '_Array',
		height: h,
		table: [parentise(tree, h - 1)],
		lengths: [length(tree)]
	};
}

// Emphasizes blood brotherhood beneath two trees.
function siblise(a, b)
{
	return {
		ctor: '_Array',
		height: a.height + 1,
		table: [a, b],
		lengths: [length(a), length(a) + length(b)]
	};
}

function toJSArray(a)
{
	var jsArray = new Array(length(a));
	toJSArray_(jsArray, 0, a);
	return jsArray;
}

function toJSArray_(jsArray, i, a)
{
	for (var t = 0; t < a.table.length; t++)
	{
		if (a.height === 0)
		{
			jsArray[i + t] = a.table[t];
		}
		else
		{
			var inc = t === 0 ? 0 : a.lengths[t - 1];
			toJSArray_(jsArray, i + inc, a.table[t]);
		}
	}
}

function fromJSArray(jsArray)
{
	if (jsArray.length === 0)
	{
		return empty;
	}
	var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
	return fromJSArray_(jsArray, h, 0, jsArray.length);
}

function fromJSArray_(jsArray, h, from, to)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: jsArray.slice(from, to)
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

return {
	empty: empty,
	fromList: fromList,
	toList: toList,
	initialize: F2(initialize),
	append: F2(append),
	push: F2(push),
	slice: F3(slice),
	get: F2(get),
	set: F3(set),
	map: F2(map),
	indexedMap: F2(indexedMap),
	foldl: F3(foldl),
	foldr: F3(foldr),
	length: length,

	toJSArray: toJSArray,
	fromJSArray: fromJSArray
};

}();
var _elm_lang$core$Array$append = _elm_lang$core$Native_Array.append;
var _elm_lang$core$Array$length = _elm_lang$core$Native_Array.length;
var _elm_lang$core$Array$isEmpty = function (array) {
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Array$length(array),
		0);
};
var _elm_lang$core$Array$slice = _elm_lang$core$Native_Array.slice;
var _elm_lang$core$Array$set = _elm_lang$core$Native_Array.set;
var _elm_lang$core$Array$get = F2(
	function (i, array) {
		return ((_elm_lang$core$Native_Utils.cmp(0, i) < 1) && (_elm_lang$core$Native_Utils.cmp(
			i,
			_elm_lang$core$Native_Array.length(array)) < 0)) ? _elm_lang$core$Maybe$Just(
			A2(_elm_lang$core$Native_Array.get, i, array)) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_lang$core$Array$push = _elm_lang$core$Native_Array.push;
var _elm_lang$core$Array$empty = _elm_lang$core$Native_Array.empty;
var _elm_lang$core$Array$filter = F2(
	function (isOkay, arr) {
		var update = F2(
			function (x, xs) {
				return isOkay(x) ? A2(_elm_lang$core$Native_Array.push, x, xs) : xs;
			});
		return A3(_elm_lang$core$Native_Array.foldl, update, _elm_lang$core$Native_Array.empty, arr);
	});
var _elm_lang$core$Array$foldr = _elm_lang$core$Native_Array.foldr;
var _elm_lang$core$Array$foldl = _elm_lang$core$Native_Array.foldl;
var _elm_lang$core$Array$indexedMap = _elm_lang$core$Native_Array.indexedMap;
var _elm_lang$core$Array$map = _elm_lang$core$Native_Array.map;
var _elm_lang$core$Array$toIndexedList = function (array) {
	return A3(
		_elm_lang$core$List$map2,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}),
		_elm_lang$core$Native_List.range(
			0,
			_elm_lang$core$Native_Array.length(array) - 1),
		_elm_lang$core$Native_Array.toList(array));
};
var _elm_lang$core$Array$toList = _elm_lang$core$Native_Array.toList;
var _elm_lang$core$Array$fromList = _elm_lang$core$Native_Array.fromList;
var _elm_lang$core$Array$initialize = _elm_lang$core$Native_Array.initialize;
var _elm_lang$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			_elm_lang$core$Array$initialize,
			n,
			_elm_lang$core$Basics$always(e));
	});
var _elm_lang$core$Array$Array = {ctor: 'Array'};

//import Maybe, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_String = function() {

function isEmpty(str)
{
	return str.length === 0;
}
function cons(chr, str)
{
	return chr + str;
}
function uncons(str)
{
	var hd = str[0];
	if (hd)
	{
		return _elm_lang$core$Maybe$Just(_elm_lang$core$Native_Utils.Tuple2(_elm_lang$core$Native_Utils.chr(hd), str.slice(1)));
	}
	return _elm_lang$core$Maybe$Nothing;
}
function append(a, b)
{
	return a + b;
}
function concat(strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join('');
}
function length(str)
{
	return str.length;
}
function map(f, str)
{
	var out = str.split('');
	for (var i = out.length; i--; )
	{
		out[i] = f(_elm_lang$core$Native_Utils.chr(out[i]));
	}
	return out.join('');
}
function filter(pred, str)
{
	return str.split('').map(_elm_lang$core$Native_Utils.chr).filter(pred).join('');
}
function reverse(str)
{
	return str.split('').reverse().join('');
}
function foldl(f, b, str)
{
	var len = str.length;
	for (var i = 0; i < len; ++i)
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function foldr(f, b, str)
{
	for (var i = str.length; i--; )
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function split(sep, str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(sep));
}
function join(sep, strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join(sep);
}
function repeat(n, str)
{
	var result = '';
	while (n > 0)
	{
		if (n & 1)
		{
			result += str;
		}
		n >>= 1, str += str;
	}
	return result;
}
function slice(start, end, str)
{
	return str.slice(start, end);
}
function left(n, str)
{
	return n < 1 ? '' : str.slice(0, n);
}
function right(n, str)
{
	return n < 1 ? '' : str.slice(-n);
}
function dropLeft(n, str)
{
	return n < 1 ? str : str.slice(n);
}
function dropRight(n, str)
{
	return n < 1 ? str : str.slice(0, -n);
}
function pad(n, chr, str)
{
	var half = (n - str.length) / 2;
	return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
}
function padRight(n, chr, str)
{
	return str + repeat(n - str.length, chr);
}
function padLeft(n, chr, str)
{
	return repeat(n - str.length, chr) + str;
}

function trim(str)
{
	return str.trim();
}
function trimLeft(str)
{
	return str.replace(/^\s+/, '');
}
function trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function words(str)
{
	return _elm_lang$core$Native_List.fromArray(str.trim().split(/\s+/g));
}
function lines(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(/\r\n|\r|\n/g));
}

function toUpper(str)
{
	return str.toUpperCase();
}
function toLower(str)
{
	return str.toLowerCase();
}

function any(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return true;
		}
	}
	return false;
}
function all(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (!pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return false;
		}
	}
	return true;
}

function contains(sub, str)
{
	return str.indexOf(sub) > -1;
}
function startsWith(sub, str)
{
	return str.indexOf(sub) === 0;
}
function endsWith(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
}
function indexes(sub, str)
{
	var subLen = sub.length;
	
	if (subLen < 1)
	{
		return _elm_lang$core$Native_List.Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}	
	
	return _elm_lang$core$Native_List.fromArray(is);
}

function toInt(s)
{
	var len = s.length;
	if (len === 0)
	{
		return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int" );
	}
	var start = 0;
	if (s[0] === '-')
	{
		if (len === 1)
		{
			return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int" );
		}
		start = 1;
	}
	for (var i = start; i < len; ++i)
	{
		var c = s[i];
		if (c < '0' || '9' < c)
		{
			return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int" );
		}
	}
	return _elm_lang$core$Result$Ok(parseInt(s, 10));
}

function toFloat(s)
{
	var len = s.length;
	if (len === 0)
	{
		return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float" );
	}
	var start = 0;
	if (s[0] === '-')
	{
		if (len === 1)
		{
			return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float" );
		}
		start = 1;
	}
	var dotCount = 0;
	for (var i = start; i < len; ++i)
	{
		var c = s[i];
		if ('0' <= c && c <= '9')
		{
			continue;
		}
		if (c === '.')
		{
			dotCount += 1;
			if (dotCount <= 1)
			{
				continue;
			}
		}
		return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float" );
	}
	return _elm_lang$core$Result$Ok(parseFloat(s));
}

function toList(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split('').map(_elm_lang$core$Native_Utils.chr));
}
function fromList(chars)
{
	return _elm_lang$core$Native_List.toArray(chars).join('');
}

return {
	isEmpty: isEmpty,
	cons: F2(cons),
	uncons: uncons,
	append: F2(append),
	concat: concat,
	length: length,
	map: F2(map),
	filter: F2(filter),
	reverse: reverse,
	foldl: F3(foldl),
	foldr: F3(foldr),

	split: F2(split),
	join: F2(join),
	repeat: F2(repeat),

	slice: F3(slice),
	left: F2(left),
	right: F2(right),
	dropLeft: F2(dropLeft),
	dropRight: F2(dropRight),

	pad: F3(pad),
	padLeft: F3(padLeft),
	padRight: F3(padRight),

	trim: trim,
	trimLeft: trimLeft,
	trimRight: trimRight,

	words: words,
	lines: lines,

	toUpper: toUpper,
	toLower: toLower,

	any: F2(any),
	all: F2(all),

	contains: F2(contains),
	startsWith: F2(startsWith),
	endsWith: F2(endsWith),
	indexes: F2(indexes),

	toInt: toInt,
	toFloat: toFloat,
	toList: toList,
	fromList: fromList
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Char = function() {

return {
	fromCode: function(c) { return _elm_lang$core$Native_Utils.chr(String.fromCharCode(c)); },
	toCode: function(c) { return c.charCodeAt(0); },
	toUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toUpperCase()); },
	toLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLowerCase()); },
	toLocaleUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleUpperCase()); },
	toLocaleLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleLowerCase()); }
};

}();
var _elm_lang$core$Char$fromCode = _elm_lang$core$Native_Char.fromCode;
var _elm_lang$core$Char$toCode = _elm_lang$core$Native_Char.toCode;
var _elm_lang$core$Char$toLocaleLower = _elm_lang$core$Native_Char.toLocaleLower;
var _elm_lang$core$Char$toLocaleUpper = _elm_lang$core$Native_Char.toLocaleUpper;
var _elm_lang$core$Char$toLower = _elm_lang$core$Native_Char.toLower;
var _elm_lang$core$Char$toUpper = _elm_lang$core$Native_Char.toUpper;
var _elm_lang$core$Char$isBetween = F3(
	function (low, high, $char) {
		var code = _elm_lang$core$Char$toCode($char);
		return (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(low)) > -1) && (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(high)) < 1);
	});
var _elm_lang$core$Char$isUpper = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('A'),
	_elm_lang$core$Native_Utils.chr('Z'));
var _elm_lang$core$Char$isLower = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('a'),
	_elm_lang$core$Native_Utils.chr('z'));
var _elm_lang$core$Char$isDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('9'));
var _elm_lang$core$Char$isOctDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('7'));
var _elm_lang$core$Char$isHexDigit = function ($char) {
	return _elm_lang$core$Char$isDigit($char) || (A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('a'),
		_elm_lang$core$Native_Utils.chr('f'),
		$char) || A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('A'),
		_elm_lang$core$Native_Utils.chr('F'),
		$char));
};

var _elm_lang$core$String$fromList = _elm_lang$core$Native_String.fromList;
var _elm_lang$core$String$toList = _elm_lang$core$Native_String.toList;
var _elm_lang$core$String$toFloat = _elm_lang$core$Native_String.toFloat;
var _elm_lang$core$String$toInt = _elm_lang$core$Native_String.toInt;
var _elm_lang$core$String$indices = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$indexes = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$endsWith = _elm_lang$core$Native_String.endsWith;
var _elm_lang$core$String$startsWith = _elm_lang$core$Native_String.startsWith;
var _elm_lang$core$String$contains = _elm_lang$core$Native_String.contains;
var _elm_lang$core$String$all = _elm_lang$core$Native_String.all;
var _elm_lang$core$String$any = _elm_lang$core$Native_String.any;
var _elm_lang$core$String$toLower = _elm_lang$core$Native_String.toLower;
var _elm_lang$core$String$toUpper = _elm_lang$core$Native_String.toUpper;
var _elm_lang$core$String$lines = _elm_lang$core$Native_String.lines;
var _elm_lang$core$String$words = _elm_lang$core$Native_String.words;
var _elm_lang$core$String$trimRight = _elm_lang$core$Native_String.trimRight;
var _elm_lang$core$String$trimLeft = _elm_lang$core$Native_String.trimLeft;
var _elm_lang$core$String$trim = _elm_lang$core$Native_String.trim;
var _elm_lang$core$String$padRight = _elm_lang$core$Native_String.padRight;
var _elm_lang$core$String$padLeft = _elm_lang$core$Native_String.padLeft;
var _elm_lang$core$String$pad = _elm_lang$core$Native_String.pad;
var _elm_lang$core$String$dropRight = _elm_lang$core$Native_String.dropRight;
var _elm_lang$core$String$dropLeft = _elm_lang$core$Native_String.dropLeft;
var _elm_lang$core$String$right = _elm_lang$core$Native_String.right;
var _elm_lang$core$String$left = _elm_lang$core$Native_String.left;
var _elm_lang$core$String$slice = _elm_lang$core$Native_String.slice;
var _elm_lang$core$String$repeat = _elm_lang$core$Native_String.repeat;
var _elm_lang$core$String$join = _elm_lang$core$Native_String.join;
var _elm_lang$core$String$split = _elm_lang$core$Native_String.split;
var _elm_lang$core$String$foldr = _elm_lang$core$Native_String.foldr;
var _elm_lang$core$String$foldl = _elm_lang$core$Native_String.foldl;
var _elm_lang$core$String$reverse = _elm_lang$core$Native_String.reverse;
var _elm_lang$core$String$filter = _elm_lang$core$Native_String.filter;
var _elm_lang$core$String$map = _elm_lang$core$Native_String.map;
var _elm_lang$core$String$length = _elm_lang$core$Native_String.length;
var _elm_lang$core$String$concat = _elm_lang$core$Native_String.concat;
var _elm_lang$core$String$append = _elm_lang$core$Native_String.append;
var _elm_lang$core$String$uncons = _elm_lang$core$Native_String.uncons;
var _elm_lang$core$String$cons = _elm_lang$core$Native_String.cons;
var _elm_lang$core$String$fromChar = function ($char) {
	return A2(_elm_lang$core$String$cons, $char, '');
};
var _elm_lang$core$String$isEmpty = _elm_lang$core$Native_String.isEmpty;

var _elm_lang$core$Dict$foldr = F3(
	function (f, acc, t) {
		foldr:
		while (true) {
			var _p0 = t;
			if (_p0.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v1 = f,
					_v2 = A3(
					f,
					_p0._1,
					_p0._2,
					A3(_elm_lang$core$Dict$foldr, f, acc, _p0._4)),
					_v3 = _p0._3;
				f = _v1;
				acc = _v2;
				t = _v3;
				continue foldr;
			}
		}
	});
var _elm_lang$core$Dict$keys = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(_elm_lang$core$List_ops['::'], key, keyList);
			}),
		_elm_lang$core$Native_List.fromArray(
			[]),
		dict);
};
var _elm_lang$core$Dict$values = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2(_elm_lang$core$List_ops['::'], value, valueList);
			}),
		_elm_lang$core$Native_List.fromArray(
			[]),
		dict);
};
var _elm_lang$core$Dict$toList = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					_elm_lang$core$List_ops['::'],
					{ctor: '_Tuple2', _0: key, _1: value},
					list);
			}),
		_elm_lang$core$Native_List.fromArray(
			[]),
		dict);
};
var _elm_lang$core$Dict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			var _p1 = dict;
			if (_p1.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v5 = f,
					_v6 = A3(
					f,
					_p1._1,
					_p1._2,
					A3(_elm_lang$core$Dict$foldl, f, acc, _p1._3)),
					_v7 = _p1._4;
				f = _v5;
				acc = _v6;
				dict = _v7;
				continue foldl;
			}
		}
	});
var _elm_lang$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _p2) {
				stepState:
				while (true) {
					var _p3 = _p2;
					var _p9 = _p3._1;
					var _p8 = _p3._0;
					var _p4 = _p8;
					if (_p4.ctor === '[]') {
						return {
							ctor: '_Tuple2',
							_0: _p8,
							_1: A3(rightStep, rKey, rValue, _p9)
						};
					} else {
						var _p7 = _p4._1;
						var _p6 = _p4._0._1;
						var _p5 = _p4._0._0;
						if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) < 0) {
							var _v10 = rKey,
								_v11 = rValue,
								_v12 = {
								ctor: '_Tuple2',
								_0: _p7,
								_1: A3(leftStep, _p5, _p6, _p9)
							};
							rKey = _v10;
							rValue = _v11;
							_p2 = _v12;
							continue stepState;
						} else {
							if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) > 0) {
								return {
									ctor: '_Tuple2',
									_0: _p8,
									_1: A3(rightStep, rKey, rValue, _p9)
								};
							} else {
								return {
									ctor: '_Tuple2',
									_0: _p7,
									_1: A4(bothStep, _p5, _p6, rValue, _p9)
								};
							}
						}
					}
				}
			});
		var _p10 = A3(
			_elm_lang$core$Dict$foldl,
			stepState,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Dict$toList(leftDict),
				_1: initialResult
			},
			rightDict);
		var leftovers = _p10._0;
		var intermediateResult = _p10._1;
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p11, result) {
					var _p12 = _p11;
					return A3(leftStep, _p12._0, _p12._1, result);
				}),
			intermediateResult,
			leftovers);
	});
var _elm_lang$core$Dict$reportRemBug = F4(
	function (msg, c, lgot, rgot) {
		return _elm_lang$core$Native_Debug.crash(
			_elm_lang$core$String$concat(
				_elm_lang$core$Native_List.fromArray(
					[
						'Internal red-black tree invariant violated, expected ',
						msg,
						' and got ',
						_elm_lang$core$Basics$toString(c),
						'/',
						lgot,
						'/',
						rgot,
						'\nPlease report this bug to <https://github.com/elm-lang/core/issues>'
					])));
	});
var _elm_lang$core$Dict$isBBlack = function (dict) {
	var _p13 = dict;
	_v14_2:
	do {
		if (_p13.ctor === 'RBNode_elm_builtin') {
			if (_p13._0.ctor === 'BBlack') {
				return true;
			} else {
				break _v14_2;
			}
		} else {
			if (_p13._0.ctor === 'LBBlack') {
				return true;
			} else {
				break _v14_2;
			}
		}
	} while(false);
	return false;
};
var _elm_lang$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			var _p14 = dict;
			if (_p14.ctor === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var _v16 = A2(_elm_lang$core$Dict$sizeHelp, n + 1, _p14._4),
					_v17 = _p14._3;
				n = _v16;
				dict = _v17;
				continue sizeHelp;
			}
		}
	});
var _elm_lang$core$Dict$size = function (dict) {
	return A2(_elm_lang$core$Dict$sizeHelp, 0, dict);
};
var _elm_lang$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			var _p15 = dict;
			if (_p15.ctor === 'RBEmpty_elm_builtin') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p16 = A2(_elm_lang$core$Basics$compare, targetKey, _p15._1);
				switch (_p16.ctor) {
					case 'LT':
						var _v20 = targetKey,
							_v21 = _p15._3;
						targetKey = _v20;
						dict = _v21;
						continue get;
					case 'EQ':
						return _elm_lang$core$Maybe$Just(_p15._2);
					default:
						var _v22 = targetKey,
							_v23 = _p15._4;
						targetKey = _v22;
						dict = _v23;
						continue get;
				}
			}
		}
	});
var _elm_lang$core$Dict$member = F2(
	function (key, dict) {
		var _p17 = A2(_elm_lang$core$Dict$get, key, dict);
		if (_p17.ctor === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var _elm_lang$core$Dict$maxWithDefault = F3(
	function (k, v, r) {
		maxWithDefault:
		while (true) {
			var _p18 = r;
			if (_p18.ctor === 'RBEmpty_elm_builtin') {
				return {ctor: '_Tuple2', _0: k, _1: v};
			} else {
				var _v26 = _p18._1,
					_v27 = _p18._2,
					_v28 = _p18._4;
				k = _v26;
				v = _v27;
				r = _v28;
				continue maxWithDefault;
			}
		}
	});
var _elm_lang$core$Dict$NBlack = {ctor: 'NBlack'};
var _elm_lang$core$Dict$BBlack = {ctor: 'BBlack'};
var _elm_lang$core$Dict$Black = {ctor: 'Black'};
var _elm_lang$core$Dict$blackish = function (t) {
	var _p19 = t;
	if (_p19.ctor === 'RBNode_elm_builtin') {
		var _p20 = _p19._0;
		return _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$Black) || _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$BBlack);
	} else {
		return true;
	}
};
var _elm_lang$core$Dict$Red = {ctor: 'Red'};
var _elm_lang$core$Dict$moreBlack = function (color) {
	var _p21 = color;
	switch (_p21.ctor) {
		case 'Black':
			return _elm_lang$core$Dict$BBlack;
		case 'Red':
			return _elm_lang$core$Dict$Black;
		case 'NBlack':
			return _elm_lang$core$Dict$Red;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a double black node more black!');
	}
};
var _elm_lang$core$Dict$lessBlack = function (color) {
	var _p22 = color;
	switch (_p22.ctor) {
		case 'BBlack':
			return _elm_lang$core$Dict$Black;
		case 'Black':
			return _elm_lang$core$Dict$Red;
		case 'Red':
			return _elm_lang$core$Dict$NBlack;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a negative black node less black!');
	}
};
var _elm_lang$core$Dict$LBBlack = {ctor: 'LBBlack'};
var _elm_lang$core$Dict$LBlack = {ctor: 'LBlack'};
var _elm_lang$core$Dict$RBEmpty_elm_builtin = function (a) {
	return {ctor: 'RBEmpty_elm_builtin', _0: a};
};
var _elm_lang$core$Dict$empty = _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
var _elm_lang$core$Dict$isEmpty = function (dict) {
	return _elm_lang$core$Native_Utils.eq(dict, _elm_lang$core$Dict$empty);
};
var _elm_lang$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {ctor: 'RBNode_elm_builtin', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Dict$ensureBlackRoot = function (dict) {
	var _p23 = dict;
	if ((_p23.ctor === 'RBNode_elm_builtin') && (_p23._0.ctor === 'Red')) {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p23._1, _p23._2, _p23._3, _p23._4);
	} else {
		return dict;
	}
};
var _elm_lang$core$Dict$lessBlackTree = function (dict) {
	var _p24 = dict;
	if (_p24.ctor === 'RBNode_elm_builtin') {
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$lessBlack(_p24._0),
			_p24._1,
			_p24._2,
			_p24._3,
			_p24._4);
	} else {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	}
};
var _elm_lang$core$Dict$balancedTree = function (col) {
	return function (xk) {
		return function (xv) {
			return function (yk) {
				return function (yv) {
					return function (zk) {
						return function (zv) {
							return function (a) {
								return function (b) {
									return function (c) {
										return function (d) {
											return A5(
												_elm_lang$core$Dict$RBNode_elm_builtin,
												_elm_lang$core$Dict$lessBlack(col),
												yk,
												yv,
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, xk, xv, a, b),
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, zk, zv, c, d));
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _elm_lang$core$Dict$blacken = function (t) {
	var _p25 = t;
	if (_p25.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p25._1, _p25._2, _p25._3, _p25._4);
	}
};
var _elm_lang$core$Dict$redden = function (t) {
	var _p26 = t;
	if (_p26.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Native_Debug.crash('can\'t make a Leaf red');
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, _p26._1, _p26._2, _p26._3, _p26._4);
	}
};
var _elm_lang$core$Dict$balanceHelp = function (tree) {
	var _p27 = tree;
	_v36_6:
	do {
		_v36_5:
		do {
			_v36_4:
			do {
				_v36_3:
				do {
					_v36_2:
					do {
						_v36_1:
						do {
							_v36_0:
							do {
								if (_p27.ctor === 'RBNode_elm_builtin') {
									if (_p27._3.ctor === 'RBNode_elm_builtin') {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._3._0.ctor) {
												case 'Red':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																		break _v36_2;
																	} else {
																		if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																			break _v36_3;
																		} else {
																			break _v36_6;
																		}
																	}
																}
															}
														case 'NBlack':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																		break _v36_4;
																	} else {
																		break _v36_6;
																	}
																}
															}
														default:
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	break _v36_6;
																}
															}
													}
												case 'NBlack':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															}
														case 'NBlack':
															if (_p27._0.ctor === 'BBlack') {
																if ((((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																	break _v36_4;
																} else {
																	if ((((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															} else {
																break _v36_6;
															}
														default:
															if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																break _v36_5;
															} else {
																break _v36_6;
															}
													}
												default:
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	break _v36_6;
																}
															}
														case 'NBlack':
															if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																break _v36_4;
															} else {
																break _v36_6;
															}
														default:
															break _v36_6;
													}
											}
										} else {
											switch (_p27._3._0.ctor) {
												case 'Red':
													if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
														break _v36_0;
													} else {
														if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
															break _v36_1;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
														break _v36_5;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										}
									} else {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._4._0.ctor) {
												case 'Red':
													if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
														break _v36_2;
													} else {
														if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
															break _v36_3;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
														break _v36_4;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										} else {
											break _v36_6;
										}
									}
								} else {
									break _v36_6;
								}
							} while(false);
							return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._3._1)(_p27._3._3._2)(_p27._3._1)(_p27._3._2)(_p27._1)(_p27._2)(_p27._3._3._3)(_p27._3._3._4)(_p27._3._4)(_p27._4);
						} while(false);
						return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._1)(_p27._3._2)(_p27._3._4._1)(_p27._3._4._2)(_p27._1)(_p27._2)(_p27._3._3)(_p27._3._4._3)(_p27._3._4._4)(_p27._4);
					} while(false);
					return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._3._1)(_p27._4._3._2)(_p27._4._1)(_p27._4._2)(_p27._3)(_p27._4._3._3)(_p27._4._3._4)(_p27._4._4);
				} while(false);
				return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._1)(_p27._4._2)(_p27._4._4._1)(_p27._4._4._2)(_p27._3)(_p27._4._3)(_p27._4._4._3)(_p27._4._4._4);
			} while(false);
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_elm_lang$core$Dict$Black,
				_p27._4._3._1,
				_p27._4._3._2,
				A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3, _p27._4._3._3),
				A5(
					_elm_lang$core$Dict$balance,
					_elm_lang$core$Dict$Black,
					_p27._4._1,
					_p27._4._2,
					_p27._4._3._4,
					_elm_lang$core$Dict$redden(_p27._4._4)));
		} while(false);
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$Black,
			_p27._3._4._1,
			_p27._3._4._2,
			A5(
				_elm_lang$core$Dict$balance,
				_elm_lang$core$Dict$Black,
				_p27._3._1,
				_p27._3._2,
				_elm_lang$core$Dict$redden(_p27._3._3),
				_p27._3._4._3),
			A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3._4._4, _p27._4));
	} while(false);
	return tree;
};
var _elm_lang$core$Dict$balance = F5(
	function (c, k, v, l, r) {
		var tree = A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
		return _elm_lang$core$Dict$blackish(tree) ? _elm_lang$core$Dict$balanceHelp(tree) : tree;
	});
var _elm_lang$core$Dict$bubble = F5(
	function (c, k, v, l, r) {
		return (_elm_lang$core$Dict$isBBlack(l) || _elm_lang$core$Dict$isBBlack(r)) ? A5(
			_elm_lang$core$Dict$balance,
			_elm_lang$core$Dict$moreBlack(c),
			k,
			v,
			_elm_lang$core$Dict$lessBlackTree(l),
			_elm_lang$core$Dict$lessBlackTree(r)) : A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
	});
var _elm_lang$core$Dict$removeMax = F5(
	function (c, k, v, l, r) {
		var _p28 = r;
		if (_p28.ctor === 'RBEmpty_elm_builtin') {
			return A3(_elm_lang$core$Dict$rem, c, l, r);
		} else {
			return A5(
				_elm_lang$core$Dict$bubble,
				c,
				k,
				v,
				l,
				A5(_elm_lang$core$Dict$removeMax, _p28._0, _p28._1, _p28._2, _p28._3, _p28._4));
		}
	});
var _elm_lang$core$Dict$rem = F3(
	function (c, l, r) {
		var _p29 = {ctor: '_Tuple2', _0: l, _1: r};
		if (_p29._0.ctor === 'RBEmpty_elm_builtin') {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p30 = c;
				switch (_p30.ctor) {
					case 'Red':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
					case 'Black':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBBlack);
					default:
						return _elm_lang$core$Native_Debug.crash('cannot have bblack or nblack nodes at this point');
				}
			} else {
				var _p33 = _p29._1._0;
				var _p32 = _p29._0._0;
				var _p31 = {ctor: '_Tuple3', _0: c, _1: _p32, _2: _p33};
				if ((((_p31.ctor === '_Tuple3') && (_p31._0.ctor === 'Black')) && (_p31._1.ctor === 'LBlack')) && (_p31._2.ctor === 'Red')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._1._1, _p29._1._2, _p29._1._3, _p29._1._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/LBlack/Red',
						c,
						_elm_lang$core$Basics$toString(_p32),
						_elm_lang$core$Basics$toString(_p33));
				}
			}
		} else {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p36 = _p29._1._0;
				var _p35 = _p29._0._0;
				var _p34 = {ctor: '_Tuple3', _0: c, _1: _p35, _2: _p36};
				if ((((_p34.ctor === '_Tuple3') && (_p34._0.ctor === 'Black')) && (_p34._1.ctor === 'Red')) && (_p34._2.ctor === 'LBlack')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._0._1, _p29._0._2, _p29._0._3, _p29._0._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/Red/LBlack',
						c,
						_elm_lang$core$Basics$toString(_p35),
						_elm_lang$core$Basics$toString(_p36));
				}
			} else {
				var _p40 = _p29._0._2;
				var _p39 = _p29._0._4;
				var _p38 = _p29._0._1;
				var l$ = A5(_elm_lang$core$Dict$removeMax, _p29._0._0, _p38, _p40, _p29._0._3, _p39);
				var _p37 = A3(_elm_lang$core$Dict$maxWithDefault, _p38, _p40, _p39);
				var k = _p37._0;
				var v = _p37._1;
				return A5(_elm_lang$core$Dict$bubble, c, k, v, l$, r);
			}
		}
	});
var _elm_lang$core$Dict$map = F2(
	function (f, dict) {
		var _p41 = dict;
		if (_p41.ctor === 'RBEmpty_elm_builtin') {
			return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
		} else {
			var _p42 = _p41._1;
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_p41._0,
				_p42,
				A2(f, _p42, _p41._2),
				A2(_elm_lang$core$Dict$map, f, _p41._3),
				A2(_elm_lang$core$Dict$map, f, _p41._4));
		}
	});
var _elm_lang$core$Dict$Same = {ctor: 'Same'};
var _elm_lang$core$Dict$Remove = {ctor: 'Remove'};
var _elm_lang$core$Dict$Insert = {ctor: 'Insert'};
var _elm_lang$core$Dict$update = F3(
	function (k, alter, dict) {
		var up = function (dict) {
			var _p43 = dict;
			if (_p43.ctor === 'RBEmpty_elm_builtin') {
				var _p44 = alter(_elm_lang$core$Maybe$Nothing);
				if (_p44.ctor === 'Nothing') {
					return {ctor: '_Tuple2', _0: _elm_lang$core$Dict$Same, _1: _elm_lang$core$Dict$empty};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Dict$Insert,
						_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, k, _p44._0, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty)
					};
				}
			} else {
				var _p55 = _p43._2;
				var _p54 = _p43._4;
				var _p53 = _p43._3;
				var _p52 = _p43._1;
				var _p51 = _p43._0;
				var _p45 = A2(_elm_lang$core$Basics$compare, k, _p52);
				switch (_p45.ctor) {
					case 'EQ':
						var _p46 = alter(
							_elm_lang$core$Maybe$Just(_p55));
						if (_p46.ctor === 'Nothing') {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Remove,
								_1: A3(_elm_lang$core$Dict$rem, _p51, _p53, _p54)
							};
						} else {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Same,
								_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p46._0, _p53, _p54)
							};
						}
					case 'LT':
						var _p47 = up(_p53);
						var flag = _p47._0;
						var newLeft = _p47._1;
						var _p48 = flag;
						switch (_p48.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, newLeft, _p54)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, newLeft, _p54)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, newLeft, _p54)
								};
						}
					default:
						var _p49 = up(_p54);
						var flag = _p49._0;
						var newRight = _p49._1;
						var _p50 = flag;
						switch (_p50.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, _p53, newRight)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, _p53, newRight)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, _p53, newRight)
								};
						}
				}
			}
		};
		var _p56 = up(dict);
		var flag = _p56._0;
		var updatedDict = _p56._1;
		var _p57 = flag;
		switch (_p57.ctor) {
			case 'Same':
				return updatedDict;
			case 'Insert':
				return _elm_lang$core$Dict$ensureBlackRoot(updatedDict);
			default:
				return _elm_lang$core$Dict$blacken(updatedDict);
		}
	});
var _elm_lang$core$Dict$insert = F3(
	function (key, value, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(
				_elm_lang$core$Maybe$Just(value)),
			dict);
	});
var _elm_lang$core$Dict$singleton = F2(
	function (key, value) {
		return A3(_elm_lang$core$Dict$insert, key, value, _elm_lang$core$Dict$empty);
	});
var _elm_lang$core$Dict$union = F2(
	function (t1, t2) {
		return A3(_elm_lang$core$Dict$foldl, _elm_lang$core$Dict$insert, t2, t1);
	});
var _elm_lang$core$Dict$filter = F2(
	function (predicate, dictionary) {
		var add = F3(
			function (key, value, dict) {
				return A2(predicate, key, value) ? A3(_elm_lang$core$Dict$insert, key, value, dict) : dict;
			});
		return A3(_elm_lang$core$Dict$foldl, add, _elm_lang$core$Dict$empty, dictionary);
	});
var _elm_lang$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Dict$filter,
			F2(
				function (k, _p58) {
					return A2(_elm_lang$core$Dict$member, k, t2);
				}),
			t1);
	});
var _elm_lang$core$Dict$partition = F2(
	function (predicate, dict) {
		var add = F3(
			function (key, value, _p59) {
				var _p60 = _p59;
				var _p62 = _p60._1;
				var _p61 = _p60._0;
				return A2(predicate, key, value) ? {
					ctor: '_Tuple2',
					_0: A3(_elm_lang$core$Dict$insert, key, value, _p61),
					_1: _p62
				} : {
					ctor: '_Tuple2',
					_0: _p61,
					_1: A3(_elm_lang$core$Dict$insert, key, value, _p62)
				};
			});
		return A3(
			_elm_lang$core$Dict$foldl,
			add,
			{ctor: '_Tuple2', _0: _elm_lang$core$Dict$empty, _1: _elm_lang$core$Dict$empty},
			dict);
	});
var _elm_lang$core$Dict$fromList = function (assocs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p63, dict) {
				var _p64 = _p63;
				return A3(_elm_lang$core$Dict$insert, _p64._0, _p64._1, dict);
			}),
		_elm_lang$core$Dict$empty,
		assocs);
};
var _elm_lang$core$Dict$remove = F2(
	function (key, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			dict);
	});
var _elm_lang$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2(_elm_lang$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});

//import Maybe, Native.Array, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_Json = function() {


// CORE DECODERS

function succeed(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'succeed',
		msg: msg
	};
}

function fail(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'fail',
		msg: msg
	};
}

function decodePrimitive(tag)
{
	return {
		ctor: '<decoder>',
		tag: tag
	};
}

function decodeContainer(tag, decoder)
{
	return {
		ctor: '<decoder>',
		tag: tag,
		decoder: decoder
	};
}

function decodeNull(value)
{
	return {
		ctor: '<decoder>',
		tag: 'null',
		value: value
	};
}

function decodeField(field, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'field',
		field: field,
		decoder: decoder
	};
}

function decodeKeyValuePairs(decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'key-value',
		decoder: decoder
	};
}

function decodeObject(f, decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'map-many',
		func: f,
		decoders: decoders
	};
}

function decodeTuple(f, decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'tuple',
		func: f,
		decoders: decoders
	};
}

function andThen(decoder, callback)
{
	return {
		ctor: '<decoder>',
		tag: 'andThen',
		decoder: decoder,
		callback: callback
	};
}

function customAndThen(decoder, callback)
{
	return {
		ctor: '<decoder>',
		tag: 'customAndThen',
		decoder: decoder,
		callback: callback
	};
}

function oneOf(decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'oneOf',
		decoders: decoders
	};
}


// DECODING OBJECTS

function decodeObject1(f, d1)
{
	return decodeObject(f, [d1]);
}

function decodeObject2(f, d1, d2)
{
	return decodeObject(f, [d1, d2]);
}

function decodeObject3(f, d1, d2, d3)
{
	return decodeObject(f, [d1, d2, d3]);
}

function decodeObject4(f, d1, d2, d3, d4)
{
	return decodeObject(f, [d1, d2, d3, d4]);
}

function decodeObject5(f, d1, d2, d3, d4, d5)
{
	return decodeObject(f, [d1, d2, d3, d4, d5]);
}

function decodeObject6(f, d1, d2, d3, d4, d5, d6)
{
	return decodeObject(f, [d1, d2, d3, d4, d5, d6]);
}

function decodeObject7(f, d1, d2, d3, d4, d5, d6, d7)
{
	return decodeObject(f, [d1, d2, d3, d4, d5, d6, d7]);
}

function decodeObject8(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return decodeObject(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
}


// DECODING TUPLES

function decodeTuple1(f, d1)
{
	return decodeTuple(f, [d1]);
}

function decodeTuple2(f, d1, d2)
{
	return decodeTuple(f, [d1, d2]);
}

function decodeTuple3(f, d1, d2, d3)
{
	return decodeTuple(f, [d1, d2, d3]);
}

function decodeTuple4(f, d1, d2, d3, d4)
{
	return decodeTuple(f, [d1, d2, d3, d4]);
}

function decodeTuple5(f, d1, d2, d3, d4, d5)
{
	return decodeTuple(f, [d1, d2, d3, d4, d5]);
}

function decodeTuple6(f, d1, d2, d3, d4, d5, d6)
{
	return decodeTuple(f, [d1, d2, d3, d4, d5, d6]);
}

function decodeTuple7(f, d1, d2, d3, d4, d5, d6, d7)
{
	return decodeTuple(f, [d1, d2, d3, d4, d5, d6, d7]);
}

function decodeTuple8(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return decodeTuple(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
}


// DECODE HELPERS

function ok(value)
{
	return { tag: 'ok', value: value };
}

function badPrimitive(type, value)
{
	return { tag: 'primitive', type: type, value: value };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badField(field, nestedProblems)
{
	return { tag: 'field', field: field, rest: nestedProblems };
}

function badOneOf(problems)
{
	return { tag: 'oneOf', problems: problems };
}

function badCustom(msg)
{
	return { tag: 'custom', msg: msg };
}

function bad(msg)
{
	return { tag: 'fail', msg: msg };
}

function badToString(problem)
{
	var context = '_';
	while (problem)
	{
		switch (problem.tag)
		{
			case 'primitive':
				return 'Expecting ' + problem.type
					+ (context === '_' ? '' : ' at ' + context)
					+ ' but instead got: ' + jsToString(problem.value);

			case 'index':
				context += '[' + problem.index + ']';
				problem = problem.rest;
				break;

			case 'field':
				context += '.' + problem.field;
				problem = problem.rest;
				break;

			case 'oneOf':
				var problems = problem.problems;
				for (var i = 0; i < problems.length; i++)
				{
					problems[i] = badToString(problems[i]);
				}
				return 'I ran into the following problems'
					+ (context === '_' ? '' : ' at ' + context)
					+ ':\n\n' + problems.join('\n');

			case 'custom':
				return 'A `customDecoder` failed'
					+ (context === '_' ? '' : ' at ' + context)
					+ ' with the message: ' + problem.msg;

			case 'fail':
				return 'I ran into a `fail` decoder'
					+ (context === '_' ? '' : ' at ' + context)
					+ ': ' + problem.msg;
		}
	}
}

function jsToString(value)
{
	return value === undefined
		? 'undefined'
		: JSON.stringify(value);
}


// DECODE

function runOnString(decoder, string)
{
	var json;
	try
	{
		json = JSON.parse(string);
	}
	catch (e)
	{
		return _elm_lang$core$Result$Err('Given an invalid JSON: ' + e.message);
	}
	return run(decoder, json);
}

function run(decoder, value)
{
	var result = runHelp(decoder, value);
	return (result.tag === 'ok')
		? _elm_lang$core$Result$Ok(result.value)
		: _elm_lang$core$Result$Err(badToString(result));
}

function runHelp(decoder, value)
{
	switch (decoder.tag)
	{
		case 'bool':
			return (typeof value === 'boolean')
				? ok(value)
				: badPrimitive('a Bool', value);

		case 'int':
			if (typeof value !== 'number') {
				return badPrimitive('an Int', value);
			}

			if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
				return ok(value);
			}

			if (isFinite(value) && !(value % 1)) {
				return ok(value);
			}

			return badPrimitive('an Int', value);

		case 'float':
			return (typeof value === 'number')
				? ok(value)
				: badPrimitive('a Float', value);

		case 'string':
			return (typeof value === 'string')
				? ok(value)
				: (value instanceof String)
					? ok(value + '')
					: badPrimitive('a String', value);

		case 'null':
			return (value === null)
				? ok(decoder.value)
				: badPrimitive('null', value);

		case 'value':
			return ok(value);

		case 'list':
			if (!(value instanceof Array))
			{
				return badPrimitive('a List', value);
			}

			var list = _elm_lang$core$Native_List.Nil;
			for (var i = value.length; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result)
				}
				list = _elm_lang$core$Native_List.Cons(result.value, list);
			}
			return ok(list);

		case 'array':
			if (!(value instanceof Array))
			{
				return badPrimitive('an Array', value);
			}

			var len = value.length;
			var array = new Array(len);
			for (var i = len; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result);
				}
				array[i] = result.value;
			}
			return ok(_elm_lang$core$Native_Array.fromJSArray(array));

		case 'maybe':
			var result = runHelp(decoder.decoder, value);
			return (result.tag === 'ok')
				? ok(_elm_lang$core$Maybe$Just(result.value))
				: ok(_elm_lang$core$Maybe$Nothing);

		case 'field':
			var field = decoder.field;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return badPrimitive('an object with a field named `' + field + '`', value);
			}

			var result = runHelp(decoder.decoder, value[field]);
			return (result.tag === 'ok')
				? result
				: badField(field, result);

		case 'key-value':
			if (typeof value !== 'object' || value === null || value instanceof Array)
			{
				return badPrimitive('an object', value);
			}

			var keyValuePairs = _elm_lang$core$Native_List.Nil;
			for (var key in value)
			{
				var result = runHelp(decoder.decoder, value[key]);
				if (result.tag !== 'ok')
				{
					return badField(key, result);
				}
				var pair = _elm_lang$core$Native_Utils.Tuple2(key, result.value);
				keyValuePairs = _elm_lang$core$Native_List.Cons(pair, keyValuePairs);
			}
			return ok(keyValuePairs);

		case 'map-many':
			var answer = decoder.func;
			var decoders = decoder.decoders;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = runHelp(decoders[i], value);
				if (result.tag !== 'ok')
				{
					return result;
				}
				answer = answer(result.value);
			}
			return ok(answer);

		case 'tuple':
			var decoders = decoder.decoders;
			var len = decoders.length;

			if ( !(value instanceof Array) || value.length !== len )
			{
				return badPrimitive('a Tuple with ' + len + ' entries', value);
			}

			var answer = decoder.func;
			for (var i = 0; i < len; i++)
			{
				var result = runHelp(decoders[i], value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result);
				}
				answer = answer(result.value);
			}
			return ok(answer);

		case 'customAndThen':
			var result = runHelp(decoder.decoder, value);
			if (result.tag !== 'ok')
			{
				return result;
			}
			var realResult = decoder.callback(result.value);
			if (realResult.ctor === 'Err')
			{
				return badCustom(realResult._0);
			}
			return ok(realResult._0);

		case 'andThen':
			var result = runHelp(decoder.decoder, value);
			return (result.tag !== 'ok')
				? result
				: runHelp(decoder.callback(result.value), value);

		case 'oneOf':
			var errors = [];
			var temp = decoder.decoders;
			while (temp.ctor !== '[]')
			{
				var result = runHelp(temp._0, value);

				if (result.tag === 'ok')
				{
					return result;
				}

				errors.push(result);

				temp = temp._1;
			}
			return badOneOf(errors);

		case 'fail':
			return bad(decoder.msg);

		case 'succeed':
			return ok(decoder.msg);
	}
}


// EQUALITY

function equality(a, b)
{
	if (a === b)
	{
		return true;
	}

	if (a.tag !== b.tag)
	{
		return false;
	}

	switch (a.tag)
	{
		case 'succeed':
		case 'fail':
			return a.msg === b.msg;

		case 'bool':
		case 'int':
		case 'float':
		case 'string':
		case 'value':
			return true;

		case 'null':
			return a.value === b.value;

		case 'list':
		case 'array':
		case 'maybe':
		case 'key-value':
			return equality(a.decoder, b.decoder);

		case 'field':
			return a.field === b.field && equality(a.decoder, b.decoder);

		case 'map-many':
		case 'tuple':
			if (a.func !== b.func)
			{
				return false;
			}
			return listEquality(a.decoders, b.decoders);

		case 'andThen':
		case 'customAndThen':
			return a.callback === b.callback && equality(a.decoder, b.decoder);

		case 'oneOf':
			return listEquality(a.decoders, b.decoders);
	}
}

function listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

function encode(indentLevel, value)
{
	return JSON.stringify(value, null, indentLevel);
}

function identity(value)
{
	return value;
}

function encodeObject(keyValuePairs)
{
	var obj = {};
	while (keyValuePairs.ctor !== '[]')
	{
		var pair = keyValuePairs._0;
		obj[pair._0] = pair._1;
		keyValuePairs = keyValuePairs._1;
	}
	return obj;
}

return {
	encode: F2(encode),
	runOnString: F2(runOnString),
	run: F2(run),

	decodeNull: decodeNull,
	decodePrimitive: decodePrimitive,
	decodeContainer: F2(decodeContainer),

	decodeField: F2(decodeField),

	decodeObject1: F2(decodeObject1),
	decodeObject2: F3(decodeObject2),
	decodeObject3: F4(decodeObject3),
	decodeObject4: F5(decodeObject4),
	decodeObject5: F6(decodeObject5),
	decodeObject6: F7(decodeObject6),
	decodeObject7: F8(decodeObject7),
	decodeObject8: F9(decodeObject8),
	decodeKeyValuePairs: decodeKeyValuePairs,

	decodeTuple1: F2(decodeTuple1),
	decodeTuple2: F3(decodeTuple2),
	decodeTuple3: F4(decodeTuple3),
	decodeTuple4: F5(decodeTuple4),
	decodeTuple5: F6(decodeTuple5),
	decodeTuple6: F7(decodeTuple6),
	decodeTuple7: F8(decodeTuple7),
	decodeTuple8: F9(decodeTuple8),

	andThen: F2(andThen),
	customAndThen: F2(customAndThen),
	fail: fail,
	succeed: succeed,
	oneOf: oneOf,

	identity: identity,
	encodeNull: null,
	encodeArray: _elm_lang$core$Native_Array.toJSArray,
	encodeList: _elm_lang$core$Native_List.toArray,
	encodeObject: encodeObject,

	equality: equality
};

}();

var _elm_lang$core$Json_Encode$list = _elm_lang$core$Native_Json.encodeList;
var _elm_lang$core$Json_Encode$array = _elm_lang$core$Native_Json.encodeArray;
var _elm_lang$core$Json_Encode$object = _elm_lang$core$Native_Json.encodeObject;
var _elm_lang$core$Json_Encode$null = _elm_lang$core$Native_Json.encodeNull;
var _elm_lang$core$Json_Encode$bool = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$float = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$int = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$string = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$encode = _elm_lang$core$Native_Json.encode;
var _elm_lang$core$Json_Encode$Value = {ctor: 'Value'};

var _elm_lang$core$Json_Decode$tuple8 = _elm_lang$core$Native_Json.decodeTuple8;
var _elm_lang$core$Json_Decode$tuple7 = _elm_lang$core$Native_Json.decodeTuple7;
var _elm_lang$core$Json_Decode$tuple6 = _elm_lang$core$Native_Json.decodeTuple6;
var _elm_lang$core$Json_Decode$tuple5 = _elm_lang$core$Native_Json.decodeTuple5;
var _elm_lang$core$Json_Decode$tuple4 = _elm_lang$core$Native_Json.decodeTuple4;
var _elm_lang$core$Json_Decode$tuple3 = _elm_lang$core$Native_Json.decodeTuple3;
var _elm_lang$core$Json_Decode$tuple2 = _elm_lang$core$Native_Json.decodeTuple2;
var _elm_lang$core$Json_Decode$tuple1 = _elm_lang$core$Native_Json.decodeTuple1;
var _elm_lang$core$Json_Decode$succeed = _elm_lang$core$Native_Json.succeed;
var _elm_lang$core$Json_Decode$fail = _elm_lang$core$Native_Json.fail;
var _elm_lang$core$Json_Decode$andThen = _elm_lang$core$Native_Json.andThen;
var _elm_lang$core$Json_Decode$customDecoder = _elm_lang$core$Native_Json.customAndThen;
var _elm_lang$core$Json_Decode$decodeValue = _elm_lang$core$Native_Json.run;
var _elm_lang$core$Json_Decode$value = _elm_lang$core$Native_Json.decodePrimitive('value');
var _elm_lang$core$Json_Decode$maybe = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'maybe', decoder);
};
var _elm_lang$core$Json_Decode$null = _elm_lang$core$Native_Json.decodeNull;
var _elm_lang$core$Json_Decode$array = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'array', decoder);
};
var _elm_lang$core$Json_Decode$list = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'list', decoder);
};
var _elm_lang$core$Json_Decode$bool = _elm_lang$core$Native_Json.decodePrimitive('bool');
var _elm_lang$core$Json_Decode$int = _elm_lang$core$Native_Json.decodePrimitive('int');
var _elm_lang$core$Json_Decode$float = _elm_lang$core$Native_Json.decodePrimitive('float');
var _elm_lang$core$Json_Decode$string = _elm_lang$core$Native_Json.decodePrimitive('string');
var _elm_lang$core$Json_Decode$oneOf = _elm_lang$core$Native_Json.oneOf;
var _elm_lang$core$Json_Decode$keyValuePairs = _elm_lang$core$Native_Json.decodeKeyValuePairs;
var _elm_lang$core$Json_Decode$object8 = _elm_lang$core$Native_Json.decodeObject8;
var _elm_lang$core$Json_Decode$object7 = _elm_lang$core$Native_Json.decodeObject7;
var _elm_lang$core$Json_Decode$object6 = _elm_lang$core$Native_Json.decodeObject6;
var _elm_lang$core$Json_Decode$object5 = _elm_lang$core$Native_Json.decodeObject5;
var _elm_lang$core$Json_Decode$object4 = _elm_lang$core$Native_Json.decodeObject4;
var _elm_lang$core$Json_Decode$object3 = _elm_lang$core$Native_Json.decodeObject3;
var _elm_lang$core$Json_Decode$object2 = _elm_lang$core$Native_Json.decodeObject2;
var _elm_lang$core$Json_Decode$object1 = _elm_lang$core$Native_Json.decodeObject1;
var _elm_lang$core$Json_Decode_ops = _elm_lang$core$Json_Decode_ops || {};
_elm_lang$core$Json_Decode_ops[':='] = _elm_lang$core$Native_Json.decodeField;
var _elm_lang$core$Json_Decode$at = F2(
	function (fields, decoder) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, y) {
					return A2(_elm_lang$core$Json_Decode_ops[':='], x, y);
				}),
			decoder,
			fields);
	});
var _elm_lang$core$Json_Decode$decodeString = _elm_lang$core$Native_Json.runOnString;
var _elm_lang$core$Json_Decode$map = _elm_lang$core$Native_Json.decodeObject1;
var _elm_lang$core$Json_Decode$dict = function (decoder) {
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$Dict$fromList,
		_elm_lang$core$Json_Decode$keyValuePairs(decoder));
};
var _elm_lang$core$Json_Decode$Decoder = {ctor: 'Decoder'};

var _debois$elm_dom$DOM$className = A2(
	_elm_lang$core$Json_Decode$at,
	_elm_lang$core$Native_List.fromArray(
		['className']),
	_elm_lang$core$Json_Decode$string);
var _debois$elm_dom$DOM$scrollTop = A2(_elm_lang$core$Json_Decode_ops[':='], 'scrollTop', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$scrollLeft = A2(_elm_lang$core$Json_Decode_ops[':='], 'scrollLeft', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetTop = A2(_elm_lang$core$Json_Decode_ops[':='], 'offsetTop', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetLeft = A2(_elm_lang$core$Json_Decode_ops[':='], 'offsetLeft', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetHeight = A2(_elm_lang$core$Json_Decode_ops[':='], 'offsetHeight', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetWidth = A2(_elm_lang$core$Json_Decode_ops[':='], 'offsetWidth', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$childNodes = function (decoder) {
	var loop = F2(
		function (idx, xs) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				_elm_lang$core$Json_Decode$maybe(
					A2(
						_elm_lang$core$Json_Decode_ops[':='],
						_elm_lang$core$Basics$toString(idx),
						decoder)),
				function (_p0) {
					return A2(
						_elm_lang$core$Maybe$withDefault,
						_elm_lang$core$Json_Decode$succeed(xs),
						A2(
							_elm_lang$core$Maybe$map,
							function (x) {
								return A2(
									loop,
									idx + 1,
									A2(_elm_lang$core$List_ops['::'], x, xs));
							},
							_p0));
				});
		});
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$List$reverse,
		A2(
			_elm_lang$core$Json_Decode_ops[':='],
			'childNodes',
			A2(
				loop,
				0,
				_elm_lang$core$Native_List.fromArray(
					[]))));
};
var _debois$elm_dom$DOM$childNode = function (idx) {
	return _elm_lang$core$Json_Decode$at(
		_elm_lang$core$Native_List.fromArray(
			[
				'childNodes',
				_elm_lang$core$Basics$toString(idx)
			]));
};
var _debois$elm_dom$DOM$parentElement = function (decoder) {
	return A2(_elm_lang$core$Json_Decode_ops[':='], 'parentElement', decoder);
};
var _debois$elm_dom$DOM$previousSibling = function (decoder) {
	return A2(_elm_lang$core$Json_Decode_ops[':='], 'previousSibling', decoder);
};
var _debois$elm_dom$DOM$nextSibling = function (decoder) {
	return A2(_elm_lang$core$Json_Decode_ops[':='], 'nextSibling', decoder);
};
var _debois$elm_dom$DOM$offsetParent = F2(
	function (x, decoder) {
		return _elm_lang$core$Json_Decode$oneOf(
			_elm_lang$core$Native_List.fromArray(
				[
					A2(
					_elm_lang$core$Json_Decode_ops[':='],
					'offsetParent',
					_elm_lang$core$Json_Decode$null(x)),
					A2(_elm_lang$core$Json_Decode_ops[':='], 'offsetParent', decoder)
				]));
	});
var _debois$elm_dom$DOM$position = F2(
	function (x, y) {
		return A2(
			_elm_lang$core$Json_Decode$andThen,
			A5(
				_elm_lang$core$Json_Decode$object4,
				F4(
					function (scrollLeft, scrollTop, offsetLeft, offsetTop) {
						return {ctor: '_Tuple2', _0: (x + offsetLeft) - scrollLeft, _1: (y + offsetTop) - scrollTop};
					}),
				_debois$elm_dom$DOM$scrollLeft,
				_debois$elm_dom$DOM$scrollTop,
				_debois$elm_dom$DOM$offsetLeft,
				_debois$elm_dom$DOM$offsetTop),
			function (_p1) {
				var _p2 = _p1;
				var _p4 = _p2._1;
				var _p3 = _p2._0;
				return A2(
					_debois$elm_dom$DOM$offsetParent,
					{ctor: '_Tuple2', _0: _p3, _1: _p4},
					A2(_debois$elm_dom$DOM$position, _p3, _p4));
			});
	});
var _debois$elm_dom$DOM$boundingClientRect = A4(
	_elm_lang$core$Json_Decode$object3,
	F3(
		function (_p5, width, height) {
			var _p6 = _p5;
			return {top: _p6._1, left: _p6._0, width: width, height: height};
		}),
	A2(_debois$elm_dom$DOM$position, 0, 0),
	_debois$elm_dom$DOM$offsetWidth,
	_debois$elm_dom$DOM$offsetHeight);
var _debois$elm_dom$DOM$target = function (decoder) {
	return A2(_elm_lang$core$Json_Decode_ops[':='], 'target', decoder);
};
var _debois$elm_dom$DOM$Rectangle = F4(
	function (a, b, c, d) {
		return {top: a, left: b, width: c, height: d};
	});

var _debois$elm_parts$Parts$map2nd = F2(
	function (f, _p0) {
		var _p1 = _p0;
		return {
			ctor: '_Tuple2',
			_0: _p1._0,
			_1: f(_p1._1)
		};
	});
var _debois$elm_parts$Parts$map1st = F2(
	function (f, _p2) {
		var _p3 = _p2;
		return {
			ctor: '_Tuple2',
			_0: f(_p3._0),
			_1: _p3._1
		};
	});
var _debois$elm_parts$Parts$generalize = F4(
	function (upd, f, m, c) {
		return _elm_lang$core$Maybe$Just(
			A2(
				_debois$elm_parts$Parts$map2nd,
				_elm_lang$core$Platform_Cmd$map(f),
				A2(upd, m, c)));
	});
var _debois$elm_parts$Parts$update = F2(
	function (_p4, c) {
		var _p5 = _p4;
		return A2(
			_elm_lang$core$Maybe$withDefault,
			{ctor: '_Tuple2', _0: c, _1: _elm_lang$core$Platform_Cmd$none},
			_p5._0(c));
	});
var _debois$elm_parts$Parts$update$ = F2(
	function (_p6, c) {
		var _p7 = _p6;
		return _p7._0(c);
	});
var _debois$elm_parts$Parts$indexed = F3(
	function (get, set, model0) {
		return {
			ctor: '_Tuple2',
			_0: F2(
				function (idx, c) {
					return A2(
						_elm_lang$core$Maybe$withDefault,
						model0,
						A2(
							_elm_lang$core$Dict$get,
							idx,
							get(c)));
				}),
			_1: F3(
				function (idx, model, c) {
					return A2(
						set,
						A3(
							_elm_lang$core$Dict$insert,
							idx,
							model,
							get(c)),
						c);
				})
		};
	});
var _debois$elm_parts$Parts$accessors = F4(
	function (get0, set0, model0, idx) {
		var _p8 = A3(_debois$elm_parts$Parts$indexed, get0, set0, model0);
		var get = _p8._0;
		var set = _p8._1;
		return {
			get: get(idx),
			set: set(idx),
			map: F2(
				function (f, c) {
					return A3(
						_elm_lang$core$Basics$flip,
						set(idx),
						c,
						f(
							A2(get, idx, c)));
				}),
			reset: function (c) {
				return function (m) {
					return A2(set0, m, c);
				}(
					A2(
						_elm_lang$core$Dict$remove,
						idx,
						get0(c)));
			}
		};
	});
var _debois$elm_parts$Parts$embedUpdate = F6(
	function (get, set, update, f, msg, c) {
		return A2(
			_elm_lang$core$Maybe$map,
			_debois$elm_parts$Parts$map1st(
				A2(_elm_lang$core$Basics$flip, set, c)),
			A3(
				update,
				f,
				msg,
				get(c)));
	});
var _debois$elm_parts$Parts$embedView = F2(
	function (get, view) {
		return function (_p9) {
			return view(
				get(_p9));
		};
	});
var _debois$elm_parts$Parts$Accessors = F4(
	function (a, b, c, d) {
		return {get: a, set: b, map: c, reset: d};
	});
var _debois$elm_parts$Parts$Msg = function (a) {
	return {ctor: 'Msg', _0: a};
};
var _debois$elm_parts$Parts$partial = F3(
	function (fwd, upd, msg) {
		return _debois$elm_parts$Parts$Msg(
			function (c) {
				return A3(
					upd,
					function (_p10) {
						return fwd(
							A3(_debois$elm_parts$Parts$partial, fwd, upd, _p10));
					},
					msg,
					c);
			});
	});
var _debois$elm_parts$Parts$pack = F5(
	function (update, get0, set0, model0, fwd) {
		var _p11 = A3(_debois$elm_parts$Parts$indexed, get0, set0, model0);
		var get = _p11._0;
		var set = _p11._1;
		return function (idx) {
			return function (_p12) {
				return fwd(
					A3(
						_debois$elm_parts$Parts$partial,
						fwd,
						A3(
							_debois$elm_parts$Parts$embedUpdate,
							get(idx),
							set(idx),
							update),
						_p12));
			};
		};
	});
var _debois$elm_parts$Parts$create = F6(
	function (view, update, get0, set0, model0, fwd) {
		var embeddedUpdate = A5(_debois$elm_parts$Parts$pack, update, get0, set0, model0, fwd);
		var get = _elm_lang$core$Basics$fst(
			A3(_debois$elm_parts$Parts$indexed, get0, set0, model0));
		return F2(
			function (idx, c) {
				return A2(
					view,
					embeddedUpdate(idx),
					A2(get, idx, c));
			});
	});
var _debois$elm_parts$Parts$pack1 = F4(
	function (update, get, set, fwd) {
		return function (_p13) {
			return fwd(
				A3(
					_debois$elm_parts$Parts$partial,
					fwd,
					A3(_debois$elm_parts$Parts$embedUpdate, get, set, update),
					_p13));
		};
	});
var _debois$elm_parts$Parts$create1 = F5(
	function (view, update, get, set, fwd) {
		var embeddedUpdate = function (_p14) {
			return fwd(
				A3(
					_debois$elm_parts$Parts$partial,
					fwd,
					A3(_debois$elm_parts$Parts$embedUpdate, get, set, update),
					_p14));
		};
		return A2(
			_debois$elm_parts$Parts$embedView,
			get,
			view(embeddedUpdate));
	});

//import Native.Json //

var _elm_lang$virtual_dom$Native_VirtualDom = function() {

var STYLE_KEY = 'STYLE';
var EVENT_KEY = 'EVENT';
var ATTR_KEY = 'ATTR';
var ATTR_NS_KEY = 'ATTR_NS';



////////////  VIRTUAL DOM NODES  ////////////


function text(string)
{
	return {
		type: 'text',
		text: string
	};
}


function node(tag)
{
	return F2(function(factList, kidList) {
		return nodeHelp(tag, factList, kidList);
	});
}


function nodeHelp(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function keyedNode(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid._1.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'keyed-node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function custom(factList, model, impl)
{
	var facts = organizeFacts(factList).facts;

	return {
		type: 'custom',
		facts: facts,
		model: model,
		impl: impl
	};
}


function map(tagger, node)
{
	return {
		type: 'tagger',
		tagger: tagger,
		node: node,
		descendantsCount: 1 + (node.descendantsCount || 0)
	};
}


function thunk(func, args, thunk)
{
	return {
		type: 'thunk',
		func: func,
		args: args,
		thunk: thunk,
		node: undefined
	};
}

function lazy(fn, a)
{
	return thunk(fn, [a], function() {
		return fn(a);
	});
}

function lazy2(fn, a, b)
{
	return thunk(fn, [a,b], function() {
		return A2(fn, a, b);
	});
}

function lazy3(fn, a, b, c)
{
	return thunk(fn, [a,b,c], function() {
		return A3(fn, a, b, c);
	});
}



// FACTS


function organizeFacts(factList)
{
	var namespace, facts = {};

	while (factList.ctor !== '[]')
	{
		var entry = factList._0;
		var key = entry.key;

		if (key === ATTR_KEY || key === ATTR_NS_KEY || key === EVENT_KEY)
		{
			var subFacts = facts[key] || {};
			subFacts[entry.realKey] = entry.value;
			facts[key] = subFacts;
		}
		else if (key === STYLE_KEY)
		{
			var styles = facts[key] || {};
			var styleList = entry.value;
			while (styleList.ctor !== '[]')
			{
				var style = styleList._0;
				styles[style._0] = style._1;
				styleList = styleList._1;
			}
			facts[key] = styles;
		}
		else if (key === 'namespace')
		{
			namespace = entry.value;
		}
		else
		{
			facts[key] = entry.value;
		}
		factList = factList._1;
	}

	return {
		facts: facts,
		namespace: namespace
	};
}



////////////  PROPERTIES AND ATTRIBUTES  ////////////


function style(value)
{
	return {
		key: STYLE_KEY,
		value: value
	};
}


function property(key, value)
{
	return {
		key: key,
		value: value
	};
}


function attribute(key, value)
{
	return {
		key: ATTR_KEY,
		realKey: key,
		value: value
	};
}


function attributeNS(namespace, key, value)
{
	return {
		key: ATTR_NS_KEY,
		realKey: key,
		value: {
			value: value,
			namespace: namespace
		}
	};
}


function on(name, options, decoder)
{
	return {
		key: EVENT_KEY,
		realKey: name,
		value: {
			options: options,
			decoder: decoder
		}
	};
}


function equalEvents(a, b)
{
	if (!a.options === b.options)
	{
		if (a.stopPropagation !== b.stopPropagation || a.preventDefault !== b.preventDefault)
		{
			return false;
		}
	}
	return _elm_lang$core$Native_Json.equality(a.decoder, b.decoder);
}



////////////  RENDERER  ////////////


function renderer(parent, tagger, initialVirtualNode)
{
	var eventNode = { tagger: tagger, parent: undefined };

	var domNode = render(initialVirtualNode, eventNode);
	parent.appendChild(domNode);

	var state = 'NO_REQUEST';
	var currentVirtualNode = initialVirtualNode;
	var nextVirtualNode = initialVirtualNode;

	function registerVirtualNode(vNode)
	{
		if (state === 'NO_REQUEST')
		{
			rAF(updateIfNeeded);
		}
		state = 'PENDING_REQUEST';
		nextVirtualNode = vNode;
	}

	function updateIfNeeded()
	{
		switch (state)
		{
			case 'NO_REQUEST':
				throw new Error(
					'Unexpected draw callback.\n' +
					'Please report this to <https://github.com/elm-lang/core/issues>.'
				);

			case 'PENDING_REQUEST':
				rAF(updateIfNeeded);
				state = 'EXTRA_REQUEST';

				var patches = diff(currentVirtualNode, nextVirtualNode);
				domNode = applyPatches(domNode, currentVirtualNode, patches, eventNode);
				currentVirtualNode = nextVirtualNode;

				return;

			case 'EXTRA_REQUEST':
				state = 'NO_REQUEST';
				return;
		}
	}

	return { update: registerVirtualNode };
}


var rAF =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(cb) { setTimeout(cb, 1000 / 60); };



////////////  RENDER  ////////////


function render(vNode, eventNode)
{
	switch (vNode.type)
	{
		case 'thunk':
			if (!vNode.node)
			{
				vNode.node = vNode.thunk();
			}
			return render(vNode.node, eventNode);

		case 'tagger':
			var subNode = vNode.node;
			var tagger = vNode.tagger;

			while (subNode.type === 'tagger')
			{
				typeof tagger !== 'object'
					? tagger = [tagger, subNode.tagger]
					: tagger.push(subNode.tagger);

				subNode = subNode.node;
			}

			var subEventRoot = {
				tagger: tagger,
				parent: eventNode
			};

			var domNode = render(subNode, subEventRoot);
			domNode.elm_event_node_ref = subEventRoot;
			return domNode;

		case 'text':
			return document.createTextNode(vNode.text);

		case 'node':
			var domNode = vNode.namespace
				? document.createElementNS(vNode.namespace, vNode.tag)
				: document.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i], eventNode));
			}

			return domNode;

		case 'keyed-node':
			var domNode = vNode.namespace
				? document.createElementNS(vNode.namespace, vNode.tag)
				: document.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i]._1, eventNode));
			}

			return domNode;

		case 'custom':
			var domNode = vNode.impl.render(vNode.model);
			applyFacts(domNode, eventNode, vNode.facts);
			return domNode;
	}
}



////////////  APPLY FACTS  ////////////


function applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		switch (key)
		{
			case STYLE_KEY:
				applyStyles(domNode, value);
				break;

			case EVENT_KEY:
				applyEvents(domNode, eventNode, value);
				break;

			case ATTR_KEY:
				applyAttrs(domNode, value);
				break;

			case ATTR_NS_KEY:
				applyAttrsNS(domNode, value);
				break;

			case 'value':
				if (domNode[key] !== value)
				{
					domNode[key] = value;
				}
				break;

			default:
				domNode[key] = value;
				break;
		}
	}
}

function applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}

function applyEvents(domNode, eventNode, events)
{
	var allHandlers = domNode.elm_handlers || {};

	for (var key in events)
	{
		var handler = allHandlers[key];
		var value = events[key];

		if (typeof value === 'undefined')
		{
			domNode.removeEventListener(key, handler);
			allHandlers[key] = undefined;
		}
		else if (typeof handler === 'undefined')
		{
			var handler = makeEventHandler(eventNode, value);
			domNode.addEventListener(key, handler);
			allHandlers[key] = handler;
		}
		else
		{
			handler.info = value;
		}
	}

	domNode.elm_handlers = allHandlers;
}

function makeEventHandler(eventNode, info)
{
	function eventHandler(event)
	{
		var info = eventHandler.info;

		var value = A2(_elm_lang$core$Native_Json.run, info.decoder, event);

		if (value.ctor === 'Ok')
		{
			var options = info.options;
			if (options.stopPropagation)
			{
				event.stopPropagation();
			}
			if (options.preventDefault)
			{
				event.preventDefault();
			}

			var message = value._0;

			var currentEventNode = eventNode;
			while (currentEventNode)
			{
				var tagger = currentEventNode.tagger;
				if (typeof tagger === 'function')
				{
					message = tagger(message);
				}
				else
				{
					for (var i = tagger.length; i--; )
					{
						message = tagger[i](message);
					}
				}
				currentEventNode = currentEventNode.parent;
			}
		}
	};

	eventHandler.info = info;

	return eventHandler;
}

function applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		if (typeof value === 'undefined')
		{
			domNode.removeAttribute(key);
		}
		else
		{
			domNode.setAttribute(key, value);
		}
	}
}

function applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.namespace;
		var value = pair.value;

		if (typeof value === 'undefined')
		{
			domNode.removeAttributeNS(namespace, key);
		}
		else
		{
			domNode.setAttributeNS(namespace, key, value);
		}
	}
}



////////////  DIFF  ////////////


function diff(a, b)
{
	var patches = [];
	diffHelp(a, b, patches, 0);
	return patches;
}


function makePatch(type, index, data)
{
	return {
		index: index,
		type: type,
		data: data,
		domNode: undefined,
		eventNode: undefined
	};
}


function diffHelp(a, b, patches, index)
{
	if (a === b)
	{
		return;
	}

	var aType = a.type;
	var bType = b.type;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (aType !== bType)
	{
		patches.push(makePatch('p-redraw', index, b));
		return;
	}

	// Now we know that both nodes are the same type.
	switch (bType)
	{
		case 'thunk':
			var aArgs = a.args;
			var bArgs = b.args;
			var i = aArgs.length;
			var same = a.func === b.func && i === bArgs.length;
			while (same && i--)
			{
				same = aArgs[i] === bArgs[i];
			}
			if (same)
			{
				b.node = a.node;
				return;
			}
			b.node = b.thunk();
			var subPatches = [];
			diffHelp(a.node, b.node, subPatches, 0);
			if (subPatches.length > 0)
			{
				patches.push(makePatch('p-thunk', index, subPatches));
			}
			return;

		case 'tagger':
			// gather nested taggers
			var aTaggers = a.tagger;
			var bTaggers = b.tagger;
			var nesting = false;

			var aSubNode = a.node;
			while (aSubNode.type === 'tagger')
			{
				nesting = true;

				typeof aTaggers !== 'object'
					? aTaggers = [aTaggers, aSubNode.tagger]
					: aTaggers.push(aSubNode.tagger);

				aSubNode = aSubNode.node;
			}

			var bSubNode = b.node;
			while (bSubNode.type === 'tagger')
			{
				nesting = true;

				typeof bTaggers !== 'object'
					? bTaggers = [bTaggers, bSubNode.tagger]
					: bTaggers.push(bSubNode.tagger);

				bSubNode = bSubNode.node;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && aTaggers.length !== bTaggers.length)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !pairwiseRefEqual(aTaggers, bTaggers) : aTaggers !== bTaggers)
			{
				patches.push(makePatch('p-tagger', index, bTaggers));
			}

			// diff everything below the taggers
			diffHelp(aSubNode, bSubNode, patches, index + 1);
			return;

		case 'text':
			if (a.text !== b.text)
			{
				patches.push(makePatch('p-text', index, b.text));
				return;
			}

			return;

		case 'node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffChildren(a, b, patches, index);
			return;

		case 'keyed-node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffKeyedChildren(a, b, patches, index);
			return;

		case 'custom':
			if (a.impl !== b.impl)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);
			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			var patch = b.impl.diff(a,b);
			if (patch)
			{
				patches.push(makePatch('p-custom', index, patch));
				return;
			}

			return;
	}
}


// assumes the incoming arrays are the same length
function pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function diffFacts(a, b, category)
{
	var diff;

	// look for changes and removals
	for (var aKey in a)
	{
		if (aKey === STYLE_KEY || aKey === EVENT_KEY || aKey === ATTR_KEY || aKey === ATTR_NS_KEY)
		{
			var subDiff = diffFacts(a[aKey], b[aKey] || {}, aKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[aKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(aKey in b))
		{
			diff = diff || {};
			diff[aKey] =
				(typeof category === 'undefined')
					? (typeof a[aKey] === 'string' ? '' : null)
					:
				(category === STYLE_KEY)
					? ''
					:
				(category === EVENT_KEY || category === ATTR_KEY)
					? undefined
					:
				{ namespace: a[aKey].namespace, value: undefined };

			continue;
		}

		var aValue = a[aKey];
		var bValue = b[aKey];

		// reference equal, so don't worry about it
		if (aValue === bValue && aKey !== 'value'
			|| category === EVENT_KEY && equalEvents(aValue, bValue))
		{
			continue;
		}

		diff = diff || {};
		diff[aKey] = bValue;
	}

	// add new stuff
	for (var bKey in b)
	{
		if (!(bKey in a))
		{
			diff = diff || {};
			diff[bKey] = b[bKey];
		}
	}

	return diff;
}


function diffChildren(aParent, bParent, patches, rootIndex)
{
	var aChildren = aParent.children;
	var bChildren = bParent.children;

	var aLen = aChildren.length;
	var bLen = bChildren.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (aLen > bLen)
	{
		patches.push(makePatch('p-remove-last', rootIndex, aLen - bLen));
	}
	else if (aLen < bLen)
	{
		patches.push(makePatch('p-append', rootIndex, bChildren.slice(aLen)));
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	var index = rootIndex;
	var minLen = aLen < bLen ? aLen : bLen;
	for (var i = 0; i < minLen; i++)
	{
		index++;
		var aChild = aChildren[i];
		diffHelp(aChild, bChildren[i], patches, index);
		index += aChild.descendantsCount || 0;
	}
}



////////////  KEYED DIFF  ////////////


function diffKeyedChildren(aParent, bParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var aChildren = aParent.children;
	var bChildren = bParent.children;
	var aLen = aChildren.length;
	var bLen = bChildren.length;
	var aIndex = 0;
	var bIndex = 0;

	var index = rootIndex;

	while (aIndex < aLen && bIndex < bLen)
	{
		var a = aChildren[aIndex];
		var b = bChildren[bIndex];

		var aKey = a._0;
		var bKey = b._0;
		var aNode = a._1;
		var bNode = b._1;

		// check if keys match

		if (aKey === bKey)
		{
			index++;
			diffHelp(aNode, bNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex++;
			bIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var aLookAhead = aIndex + 1 < aLen;
		var bLookAhead = bIndex + 1 < bLen;

		if (aLookAhead)
		{
			var aNext = aChildren[aIndex + 1];
			var aNextKey = aNext._0;
			var aNextNode = aNext._1;
			var oldMatch = bKey === aNextKey;
		}

		if (bLookAhead)
		{
			var bNext = bChildren[bIndex + 1];
			var bNextKey = bNext._0;
			var bNextNode = bNext._1;
			var newMatch = aKey === bNextKey;
		}


		// swap a and b
		if (aLookAhead && bLookAhead && newMatch && oldMatch)
		{
			index++;
			diffHelp(aNode, bNextNode, localPatches, index);
			insertNode(changes, localPatches, aKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			removeNode(changes, localPatches, aKey, aNextNode, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		// insert b
		if (bLookAhead && newMatch)
		{
			index++;
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			diffHelp(aNode, bNextNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex += 1;
			bIndex += 2;
			continue;
		}

		// remove a
		if (aLookAhead && oldMatch)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 1;
			continue;
		}

		// remove a, insert b
		if (aLookAhead && bLookAhead && aNextKey === bNextKey)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNextNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (aIndex < aLen)
	{
		index++;
		var a = aChildren[aIndex];
		var aNode = a._1;
		removeNode(changes, localPatches, a._0, aNode, index);
		index += aNode.descendantsCount || 0;
		aIndex++;
	}

	var endInserts;
	while (bIndex < bLen)
	{
		endInserts = endInserts || [];
		var b = bChildren[bIndex];
		insertNode(changes, localPatches, b._0, b._1, undefined, endInserts);
		bIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || typeof endInserts !== 'undefined')
	{
		patches.push(makePatch('p-reorder', rootIndex, {
			patches: localPatches,
			inserts: inserts,
			endInserts: endInserts
		}));
	}
}



////////////  CHANGES FROM KEYED DIFF  ////////////


var POSTFIX = '_elmW6BL';


function insertNode(changes, localPatches, key, vnode, bIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		entry = {
			tag: 'insert',
			vnode: vnode,
			index: bIndex,
			data: undefined
		};

		inserts.push({ index: bIndex, entry: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.tag === 'remove')
	{
		inserts.push({ index: bIndex, entry: entry });

		entry.tag = 'move';
		var subPatches = [];
		diffHelp(entry.vnode, vnode, subPatches, entry.index);
		entry.index = bIndex;
		entry.data.data = {
			patches: subPatches,
			entry: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	insertNode(changes, localPatches, key + POSTFIX, vnode, bIndex, inserts);
}


function removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		var patch = makePatch('p-remove', index, undefined);
		localPatches.push(patch);

		changes[key] = {
			tag: 'remove',
			vnode: vnode,
			index: index,
			data: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.tag === 'insert')
	{
		entry.tag = 'move';
		var subPatches = [];
		diffHelp(vnode, entry.vnode, subPatches, index);

		var patch = makePatch('p-remove', index, {
			patches: subPatches,
			entry: entry
		});
		localPatches.push(patch);

		return;
	}

	// this key has already been removed or moved, a duplicate!
	removeNode(changes, localPatches, key + POSTFIX, vnode, index);
}



////////////  ADD DOM NODES  ////////////
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function addDomNodes(domNode, vNode, patches, eventNode)
{
	addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.descendantsCount, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.index;

	while (index === low)
	{
		var patchType = patch.type;

		if (patchType === 'p-thunk')
		{
			addDomNodes(domNode, vNode.node, patch.data, eventNode);
		}
		else if (patchType === 'p-reorder')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var subPatches = patch.data.patches;
			if (subPatches.length > 0)
			{
				addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 'p-remove')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var data = patch.data;
			if (typeof data !== 'undefined')
			{
				data.entry.data = domNode;
				var subPatches = data.patches;
				if (subPatches.length > 0)
				{
					addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.index) > high)
		{
			return i;
		}
	}

	switch (vNode.type)
	{
		case 'tagger':
			var subNode = vNode.node;

			while (subNode.type === "tagger")
			{
				subNode = subNode.node;
			}

			return addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);

		case 'node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j];
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'keyed-node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j]._1;
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'text':
		case 'thunk':
			throw new Error('should never traverse `text` or `thunk` nodes like this');
	}
}



////////////  APPLY PATCHES  ////////////


function applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return applyPatchesHelp(rootDomNode, patches);
}

function applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.domNode
		var newNode = applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function applyPatch(domNode, patch)
{
	switch (patch.type)
	{
		case 'p-redraw':
			return applyPatchRedraw(domNode, patch.data, patch.eventNode);

		case 'p-facts':
			applyFacts(domNode, patch.eventNode, patch.data);
			return domNode;

		case 'p-text':
			domNode.replaceData(0, domNode.length, patch.data);
			return domNode;

		case 'p-thunk':
			return applyPatchesHelp(domNode, patch.data);

		case 'p-tagger':
			domNode.elm_event_node_ref.tagger = patch.data;
			return domNode;

		case 'p-remove-last':
			var i = patch.data;
			while (i--)
			{
				domNode.removeChild(domNode.lastChild);
			}
			return domNode;

		case 'p-append':
			var newNodes = patch.data;
			for (var i = 0; i < newNodes.length; i++)
			{
				domNode.appendChild(render(newNodes[i], patch.eventNode));
			}
			return domNode;

		case 'p-remove':
			var data = patch.data;
			if (typeof data === 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.entry;
			if (typeof entry.index !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.data = applyPatchesHelp(domNode, data.patches);
			return domNode;

		case 'p-reorder':
			return applyPatchReorder(domNode, patch);

		case 'p-custom':
			var impl = patch.data;
			return impl.applyPatch(domNode, impl.data);

		default:
			throw new Error('Ran into an unknown patch!');
	}
}


function applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = render(vNode, eventNode);

	if (typeof newNode.elm_event_node_ref === 'undefined')
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function applyPatchReorder(domNode, patch)
{
	var data = patch.data;

	// remove end inserts
	var frag = applyPatchReorderEndInsertsHelp(data.endInserts, patch);

	// removals
	domNode = applyPatchesHelp(domNode, data.patches);

	// inserts
	var inserts = data.inserts;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.entry;
		var node = entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode);
		domNode.insertBefore(node, domNode.childNodes[insert.index]);
	}

	// add end inserts
	if (typeof frag !== 'undefined')
	{
		domNode.appendChild(frag);
	}

	return domNode;
}


function applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (typeof endInserts === 'undefined')
	{
		return;
	}

	var frag = document.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.entry;
		frag.appendChild(entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode)
		);
	}
	return frag;
}



////////////  PROGRAMS  ////////////


function programWithFlags(details)
{
	return {
		init: details.init,
		update: details.update,
		subscriptions: details.subscriptions,
		view: details.view,
		renderer: renderer
	};
}


return {
	node: node,
	text: text,

	custom: custom,

	map: F2(map),

	on: F3(on),
	style: style,
	property: F2(property),
	attribute: F2(attribute),
	attributeNS: F3(attributeNS),

	lazy: F2(lazy),
	lazy2: F3(lazy2),
	lazy3: F4(lazy3),
	keyedNode: F3(keyedNode),

	programWithFlags: programWithFlags
};

}();
var _elm_lang$virtual_dom$VirtualDom$programWithFlags = _elm_lang$virtual_dom$Native_VirtualDom.programWithFlags;
var _elm_lang$virtual_dom$VirtualDom$keyedNode = _elm_lang$virtual_dom$Native_VirtualDom.keyedNode;
var _elm_lang$virtual_dom$VirtualDom$lazy3 = _elm_lang$virtual_dom$Native_VirtualDom.lazy3;
var _elm_lang$virtual_dom$VirtualDom$lazy2 = _elm_lang$virtual_dom$Native_VirtualDom.lazy2;
var _elm_lang$virtual_dom$VirtualDom$lazy = _elm_lang$virtual_dom$Native_VirtualDom.lazy;
var _elm_lang$virtual_dom$VirtualDom$defaultOptions = {stopPropagation: false, preventDefault: false};
var _elm_lang$virtual_dom$VirtualDom$onWithOptions = _elm_lang$virtual_dom$Native_VirtualDom.on;
var _elm_lang$virtual_dom$VirtualDom$on = F2(
	function (eventName, decoder) {
		return A3(_elm_lang$virtual_dom$VirtualDom$onWithOptions, eventName, _elm_lang$virtual_dom$VirtualDom$defaultOptions, decoder);
	});
var _elm_lang$virtual_dom$VirtualDom$style = _elm_lang$virtual_dom$Native_VirtualDom.style;
var _elm_lang$virtual_dom$VirtualDom$attributeNS = _elm_lang$virtual_dom$Native_VirtualDom.attributeNS;
var _elm_lang$virtual_dom$VirtualDom$attribute = _elm_lang$virtual_dom$Native_VirtualDom.attribute;
var _elm_lang$virtual_dom$VirtualDom$property = _elm_lang$virtual_dom$Native_VirtualDom.property;
var _elm_lang$virtual_dom$VirtualDom$map = _elm_lang$virtual_dom$Native_VirtualDom.map;
var _elm_lang$virtual_dom$VirtualDom$text = _elm_lang$virtual_dom$Native_VirtualDom.text;
var _elm_lang$virtual_dom$VirtualDom$node = _elm_lang$virtual_dom$Native_VirtualDom.node;
var _elm_lang$virtual_dom$VirtualDom$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});
var _elm_lang$virtual_dom$VirtualDom$Node = {ctor: 'Node'};
var _elm_lang$virtual_dom$VirtualDom$Property = {ctor: 'Property'};

var _elm_lang$html$Html$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$html$Html$node = _elm_lang$virtual_dom$VirtualDom$node;
var _elm_lang$html$Html$body = _elm_lang$html$Html$node('body');
var _elm_lang$html$Html$section = _elm_lang$html$Html$node('section');
var _elm_lang$html$Html$nav = _elm_lang$html$Html$node('nav');
var _elm_lang$html$Html$article = _elm_lang$html$Html$node('article');
var _elm_lang$html$Html$aside = _elm_lang$html$Html$node('aside');
var _elm_lang$html$Html$h1 = _elm_lang$html$Html$node('h1');
var _elm_lang$html$Html$h2 = _elm_lang$html$Html$node('h2');
var _elm_lang$html$Html$h3 = _elm_lang$html$Html$node('h3');
var _elm_lang$html$Html$h4 = _elm_lang$html$Html$node('h4');
var _elm_lang$html$Html$h5 = _elm_lang$html$Html$node('h5');
var _elm_lang$html$Html$h6 = _elm_lang$html$Html$node('h6');
var _elm_lang$html$Html$header = _elm_lang$html$Html$node('header');
var _elm_lang$html$Html$footer = _elm_lang$html$Html$node('footer');
var _elm_lang$html$Html$address = _elm_lang$html$Html$node('address');
var _elm_lang$html$Html$main$ = _elm_lang$html$Html$node('main');
var _elm_lang$html$Html$p = _elm_lang$html$Html$node('p');
var _elm_lang$html$Html$hr = _elm_lang$html$Html$node('hr');
var _elm_lang$html$Html$pre = _elm_lang$html$Html$node('pre');
var _elm_lang$html$Html$blockquote = _elm_lang$html$Html$node('blockquote');
var _elm_lang$html$Html$ol = _elm_lang$html$Html$node('ol');
var _elm_lang$html$Html$ul = _elm_lang$html$Html$node('ul');
var _elm_lang$html$Html$li = _elm_lang$html$Html$node('li');
var _elm_lang$html$Html$dl = _elm_lang$html$Html$node('dl');
var _elm_lang$html$Html$dt = _elm_lang$html$Html$node('dt');
var _elm_lang$html$Html$dd = _elm_lang$html$Html$node('dd');
var _elm_lang$html$Html$figure = _elm_lang$html$Html$node('figure');
var _elm_lang$html$Html$figcaption = _elm_lang$html$Html$node('figcaption');
var _elm_lang$html$Html$div = _elm_lang$html$Html$node('div');
var _elm_lang$html$Html$a = _elm_lang$html$Html$node('a');
var _elm_lang$html$Html$em = _elm_lang$html$Html$node('em');
var _elm_lang$html$Html$strong = _elm_lang$html$Html$node('strong');
var _elm_lang$html$Html$small = _elm_lang$html$Html$node('small');
var _elm_lang$html$Html$s = _elm_lang$html$Html$node('s');
var _elm_lang$html$Html$cite = _elm_lang$html$Html$node('cite');
var _elm_lang$html$Html$q = _elm_lang$html$Html$node('q');
var _elm_lang$html$Html$dfn = _elm_lang$html$Html$node('dfn');
var _elm_lang$html$Html$abbr = _elm_lang$html$Html$node('abbr');
var _elm_lang$html$Html$time = _elm_lang$html$Html$node('time');
var _elm_lang$html$Html$code = _elm_lang$html$Html$node('code');
var _elm_lang$html$Html$var = _elm_lang$html$Html$node('var');
var _elm_lang$html$Html$samp = _elm_lang$html$Html$node('samp');
var _elm_lang$html$Html$kbd = _elm_lang$html$Html$node('kbd');
var _elm_lang$html$Html$sub = _elm_lang$html$Html$node('sub');
var _elm_lang$html$Html$sup = _elm_lang$html$Html$node('sup');
var _elm_lang$html$Html$i = _elm_lang$html$Html$node('i');
var _elm_lang$html$Html$b = _elm_lang$html$Html$node('b');
var _elm_lang$html$Html$u = _elm_lang$html$Html$node('u');
var _elm_lang$html$Html$mark = _elm_lang$html$Html$node('mark');
var _elm_lang$html$Html$ruby = _elm_lang$html$Html$node('ruby');
var _elm_lang$html$Html$rt = _elm_lang$html$Html$node('rt');
var _elm_lang$html$Html$rp = _elm_lang$html$Html$node('rp');
var _elm_lang$html$Html$bdi = _elm_lang$html$Html$node('bdi');
var _elm_lang$html$Html$bdo = _elm_lang$html$Html$node('bdo');
var _elm_lang$html$Html$span = _elm_lang$html$Html$node('span');
var _elm_lang$html$Html$br = _elm_lang$html$Html$node('br');
var _elm_lang$html$Html$wbr = _elm_lang$html$Html$node('wbr');
var _elm_lang$html$Html$ins = _elm_lang$html$Html$node('ins');
var _elm_lang$html$Html$del = _elm_lang$html$Html$node('del');
var _elm_lang$html$Html$img = _elm_lang$html$Html$node('img');
var _elm_lang$html$Html$iframe = _elm_lang$html$Html$node('iframe');
var _elm_lang$html$Html$embed = _elm_lang$html$Html$node('embed');
var _elm_lang$html$Html$object = _elm_lang$html$Html$node('object');
var _elm_lang$html$Html$param = _elm_lang$html$Html$node('param');
var _elm_lang$html$Html$video = _elm_lang$html$Html$node('video');
var _elm_lang$html$Html$audio = _elm_lang$html$Html$node('audio');
var _elm_lang$html$Html$source = _elm_lang$html$Html$node('source');
var _elm_lang$html$Html$track = _elm_lang$html$Html$node('track');
var _elm_lang$html$Html$canvas = _elm_lang$html$Html$node('canvas');
var _elm_lang$html$Html$svg = _elm_lang$html$Html$node('svg');
var _elm_lang$html$Html$math = _elm_lang$html$Html$node('math');
var _elm_lang$html$Html$table = _elm_lang$html$Html$node('table');
var _elm_lang$html$Html$caption = _elm_lang$html$Html$node('caption');
var _elm_lang$html$Html$colgroup = _elm_lang$html$Html$node('colgroup');
var _elm_lang$html$Html$col = _elm_lang$html$Html$node('col');
var _elm_lang$html$Html$tbody = _elm_lang$html$Html$node('tbody');
var _elm_lang$html$Html$thead = _elm_lang$html$Html$node('thead');
var _elm_lang$html$Html$tfoot = _elm_lang$html$Html$node('tfoot');
var _elm_lang$html$Html$tr = _elm_lang$html$Html$node('tr');
var _elm_lang$html$Html$td = _elm_lang$html$Html$node('td');
var _elm_lang$html$Html$th = _elm_lang$html$Html$node('th');
var _elm_lang$html$Html$form = _elm_lang$html$Html$node('form');
var _elm_lang$html$Html$fieldset = _elm_lang$html$Html$node('fieldset');
var _elm_lang$html$Html$legend = _elm_lang$html$Html$node('legend');
var _elm_lang$html$Html$label = _elm_lang$html$Html$node('label');
var _elm_lang$html$Html$input = _elm_lang$html$Html$node('input');
var _elm_lang$html$Html$button = _elm_lang$html$Html$node('button');
var _elm_lang$html$Html$select = _elm_lang$html$Html$node('select');
var _elm_lang$html$Html$datalist = _elm_lang$html$Html$node('datalist');
var _elm_lang$html$Html$optgroup = _elm_lang$html$Html$node('optgroup');
var _elm_lang$html$Html$option = _elm_lang$html$Html$node('option');
var _elm_lang$html$Html$textarea = _elm_lang$html$Html$node('textarea');
var _elm_lang$html$Html$keygen = _elm_lang$html$Html$node('keygen');
var _elm_lang$html$Html$output = _elm_lang$html$Html$node('output');
var _elm_lang$html$Html$progress = _elm_lang$html$Html$node('progress');
var _elm_lang$html$Html$meter = _elm_lang$html$Html$node('meter');
var _elm_lang$html$Html$details = _elm_lang$html$Html$node('details');
var _elm_lang$html$Html$summary = _elm_lang$html$Html$node('summary');
var _elm_lang$html$Html$menuitem = _elm_lang$html$Html$node('menuitem');
var _elm_lang$html$Html$menu = _elm_lang$html$Html$node('menu');

var _elm_lang$html$Html_Attributes$attribute = _elm_lang$virtual_dom$VirtualDom$attribute;
var _elm_lang$html$Html_Attributes$contextmenu = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'contextmenu', value);
};
var _elm_lang$html$Html_Attributes$draggable = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'draggable', value);
};
var _elm_lang$html$Html_Attributes$list = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'list', value);
};
var _elm_lang$html$Html_Attributes$maxlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'maxlength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$datetime = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'datetime', value);
};
var _elm_lang$html$Html_Attributes$pubdate = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'pubdate', value);
};
var _elm_lang$html$Html_Attributes$colspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'colspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rowspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rowspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$property = _elm_lang$virtual_dom$VirtualDom$property;
var _elm_lang$html$Html_Attributes$stringProperty = F2(
	function (name, string) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$string(string));
	});
var _elm_lang$html$Html_Attributes$class = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'className', name);
};
var _elm_lang$html$Html_Attributes$id = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'id', name);
};
var _elm_lang$html$Html_Attributes$title = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'title', name);
};
var _elm_lang$html$Html_Attributes$accesskey = function ($char) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'accessKey',
		_elm_lang$core$String$fromChar($char));
};
var _elm_lang$html$Html_Attributes$dir = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dir', value);
};
var _elm_lang$html$Html_Attributes$dropzone = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dropzone', value);
};
var _elm_lang$html$Html_Attributes$itemprop = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'itemprop', value);
};
var _elm_lang$html$Html_Attributes$lang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'lang', value);
};
var _elm_lang$html$Html_Attributes$tabindex = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'tabIndex',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$charset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'charset', value);
};
var _elm_lang$html$Html_Attributes$content = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'content', value);
};
var _elm_lang$html$Html_Attributes$httpEquiv = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'httpEquiv', value);
};
var _elm_lang$html$Html_Attributes$language = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'language', value);
};
var _elm_lang$html$Html_Attributes$src = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'src', value);
};
var _elm_lang$html$Html_Attributes$height = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'height',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$width = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'width',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$alt = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'alt', value);
};
var _elm_lang$html$Html_Attributes$preload = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'preload', value);
};
var _elm_lang$html$Html_Attributes$poster = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'poster', value);
};
var _elm_lang$html$Html_Attributes$kind = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'kind', value);
};
var _elm_lang$html$Html_Attributes$srclang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srclang', value);
};
var _elm_lang$html$Html_Attributes$sandbox = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'sandbox', value);
};
var _elm_lang$html$Html_Attributes$srcdoc = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srcdoc', value);
};
var _elm_lang$html$Html_Attributes$type$ = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'type', value);
};
var _elm_lang$html$Html_Attributes$value = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'value', value);
};
var _elm_lang$html$Html_Attributes$defaultValue = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'defaultValue', value);
};
var _elm_lang$html$Html_Attributes$placeholder = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'placeholder', value);
};
var _elm_lang$html$Html_Attributes$accept = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'accept', value);
};
var _elm_lang$html$Html_Attributes$acceptCharset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'acceptCharset', value);
};
var _elm_lang$html$Html_Attributes$action = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'action', value);
};
var _elm_lang$html$Html_Attributes$autocomplete = function (bool) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var _elm_lang$html$Html_Attributes$autosave = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'autosave', value);
};
var _elm_lang$html$Html_Attributes$enctype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'enctype', value);
};
var _elm_lang$html$Html_Attributes$formaction = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'formAction', value);
};
var _elm_lang$html$Html_Attributes$minlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'minLength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$method = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'method', value);
};
var _elm_lang$html$Html_Attributes$name = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'name', value);
};
var _elm_lang$html$Html_Attributes$pattern = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'pattern', value);
};
var _elm_lang$html$Html_Attributes$size = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'size',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$for = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'htmlFor', value);
};
var _elm_lang$html$Html_Attributes$form = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'form', value);
};
var _elm_lang$html$Html_Attributes$max = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'max', value);
};
var _elm_lang$html$Html_Attributes$min = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'min', value);
};
var _elm_lang$html$Html_Attributes$step = function (n) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'step', n);
};
var _elm_lang$html$Html_Attributes$cols = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'cols',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rows = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'rows',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$wrap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'wrap', value);
};
var _elm_lang$html$Html_Attributes$usemap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'useMap', value);
};
var _elm_lang$html$Html_Attributes$shape = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'shape', value);
};
var _elm_lang$html$Html_Attributes$coords = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'coords', value);
};
var _elm_lang$html$Html_Attributes$challenge = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'challenge', value);
};
var _elm_lang$html$Html_Attributes$keytype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'keytype', value);
};
var _elm_lang$html$Html_Attributes$align = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'align', value);
};
var _elm_lang$html$Html_Attributes$cite = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'cite', value);
};
var _elm_lang$html$Html_Attributes$href = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'href', value);
};
var _elm_lang$html$Html_Attributes$target = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'target', value);
};
var _elm_lang$html$Html_Attributes$downloadAs = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'download', value);
};
var _elm_lang$html$Html_Attributes$hreflang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'hreflang', value);
};
var _elm_lang$html$Html_Attributes$media = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'media', value);
};
var _elm_lang$html$Html_Attributes$ping = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'ping', value);
};
var _elm_lang$html$Html_Attributes$rel = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'rel', value);
};
var _elm_lang$html$Html_Attributes$start = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'start',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$headers = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'headers', value);
};
var _elm_lang$html$Html_Attributes$scope = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'scope', value);
};
var _elm_lang$html$Html_Attributes$manifest = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'manifest', value);
};
var _elm_lang$html$Html_Attributes$boolProperty = F2(
	function (name, bool) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$bool(bool));
	});
var _elm_lang$html$Html_Attributes$hidden = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'hidden', bool);
};
var _elm_lang$html$Html_Attributes$contenteditable = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'contentEditable', bool);
};
var _elm_lang$html$Html_Attributes$spellcheck = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'spellcheck', bool);
};
var _elm_lang$html$Html_Attributes$async = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'async', bool);
};
var _elm_lang$html$Html_Attributes$defer = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'defer', bool);
};
var _elm_lang$html$Html_Attributes$scoped = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'scoped', bool);
};
var _elm_lang$html$Html_Attributes$autoplay = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autoplay', bool);
};
var _elm_lang$html$Html_Attributes$controls = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'controls', bool);
};
var _elm_lang$html$Html_Attributes$loop = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'loop', bool);
};
var _elm_lang$html$Html_Attributes$default = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'default', bool);
};
var _elm_lang$html$Html_Attributes$seamless = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'seamless', bool);
};
var _elm_lang$html$Html_Attributes$checked = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'checked', bool);
};
var _elm_lang$html$Html_Attributes$selected = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'selected', bool);
};
var _elm_lang$html$Html_Attributes$autofocus = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autofocus', bool);
};
var _elm_lang$html$Html_Attributes$disabled = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'disabled', bool);
};
var _elm_lang$html$Html_Attributes$multiple = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'multiple', bool);
};
var _elm_lang$html$Html_Attributes$novalidate = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'noValidate', bool);
};
var _elm_lang$html$Html_Attributes$readonly = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'readOnly', bool);
};
var _elm_lang$html$Html_Attributes$required = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'required', bool);
};
var _elm_lang$html$Html_Attributes$ismap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'isMap', value);
};
var _elm_lang$html$Html_Attributes$download = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'download', bool);
};
var _elm_lang$html$Html_Attributes$reversed = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'reversed', bool);
};
var _elm_lang$html$Html_Attributes$classList = function (list) {
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$String$join,
			' ',
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Basics$fst,
				A2(_elm_lang$core$List$filter, _elm_lang$core$Basics$snd, list))));
};
var _elm_lang$html$Html_Attributes$style = _elm_lang$virtual_dom$VirtualDom$style;

//import Native.Scheduler //

var _elm_lang$core$Native_Time = function() {

var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
});

function setInterval_(interval, task)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var id = setInterval(function() {
			_elm_lang$core$Native_Scheduler.rawSpawn(task);
		}, interval);

		return function() { clearInterval(id); };
	});
}

return {
	now: now,
	setInterval_: F2(setInterval_)
};

}();
var _elm_lang$core$Task$onError = _elm_lang$core$Native_Scheduler.onError;
var _elm_lang$core$Task$andThen = _elm_lang$core$Native_Scheduler.andThen;
var _elm_lang$core$Task$spawnCmd = F2(
	function (router, _p0) {
		var _p1 = _p0;
		return _elm_lang$core$Native_Scheduler.spawn(
			A2(
				_elm_lang$core$Task$andThen,
				_p1._0,
				_elm_lang$core$Platform$sendToApp(router)));
	});
var _elm_lang$core$Task$fail = _elm_lang$core$Native_Scheduler.fail;
var _elm_lang$core$Task$mapError = F2(
	function (f, task) {
		return A2(
			_elm_lang$core$Task$onError,
			task,
			function (err) {
				return _elm_lang$core$Task$fail(
					f(err));
			});
	});
var _elm_lang$core$Task$succeed = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			_elm_lang$core$Task$andThen,
			taskA,
			function (a) {
				return _elm_lang$core$Task$succeed(
					func(a));
			});
	});
var _elm_lang$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			_elm_lang$core$Task$andThen,
			taskA,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					taskB,
					function (b) {
						return _elm_lang$core$Task$succeed(
							A2(func, a, b));
					});
			});
	});
var _elm_lang$core$Task$map3 = F4(
	function (func, taskA, taskB, taskC) {
		return A2(
			_elm_lang$core$Task$andThen,
			taskA,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					taskB,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							taskC,
							function (c) {
								return _elm_lang$core$Task$succeed(
									A3(func, a, b, c));
							});
					});
			});
	});
var _elm_lang$core$Task$map4 = F5(
	function (func, taskA, taskB, taskC, taskD) {
		return A2(
			_elm_lang$core$Task$andThen,
			taskA,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					taskB,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							taskC,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									taskD,
									function (d) {
										return _elm_lang$core$Task$succeed(
											A4(func, a, b, c, d));
									});
							});
					});
			});
	});
var _elm_lang$core$Task$map5 = F6(
	function (func, taskA, taskB, taskC, taskD, taskE) {
		return A2(
			_elm_lang$core$Task$andThen,
			taskA,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					taskB,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							taskC,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									taskD,
									function (d) {
										return A2(
											_elm_lang$core$Task$andThen,
											taskE,
											function (e) {
												return _elm_lang$core$Task$succeed(
													A5(func, a, b, c, d, e));
											});
									});
							});
					});
			});
	});
var _elm_lang$core$Task$andMap = F2(
	function (taskFunc, taskValue) {
		return A2(
			_elm_lang$core$Task$andThen,
			taskFunc,
			function (func) {
				return A2(
					_elm_lang$core$Task$andThen,
					taskValue,
					function (value) {
						return _elm_lang$core$Task$succeed(
							func(value));
					});
			});
	});
var _elm_lang$core$Task$sequence = function (tasks) {
	var _p2 = tasks;
	if (_p2.ctor === '[]') {
		return _elm_lang$core$Task$succeed(
			_elm_lang$core$Native_List.fromArray(
				[]));
	} else {
		return A3(
			_elm_lang$core$Task$map2,
			F2(
				function (x, y) {
					return A2(_elm_lang$core$List_ops['::'], x, y);
				}),
			_p2._0,
			_elm_lang$core$Task$sequence(_p2._1));
	}
};
var _elm_lang$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			_elm_lang$core$Task$map,
			function (_p3) {
				return {ctor: '_Tuple0'};
			},
			_elm_lang$core$Task$sequence(
				A2(
					_elm_lang$core$List$map,
					_elm_lang$core$Task$spawnCmd(router),
					commands)));
	});
var _elm_lang$core$Task$toMaybe = function (task) {
	return A2(
		_elm_lang$core$Task$onError,
		A2(_elm_lang$core$Task$map, _elm_lang$core$Maybe$Just, task),
		function (_p4) {
			return _elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing);
		});
};
var _elm_lang$core$Task$fromMaybe = F2(
	function ($default, maybe) {
		var _p5 = maybe;
		if (_p5.ctor === 'Just') {
			return _elm_lang$core$Task$succeed(_p5._0);
		} else {
			return _elm_lang$core$Task$fail($default);
		}
	});
var _elm_lang$core$Task$toResult = function (task) {
	return A2(
		_elm_lang$core$Task$onError,
		A2(_elm_lang$core$Task$map, _elm_lang$core$Result$Ok, task),
		function (msg) {
			return _elm_lang$core$Task$succeed(
				_elm_lang$core$Result$Err(msg));
		});
};
var _elm_lang$core$Task$fromResult = function (result) {
	var _p6 = result;
	if (_p6.ctor === 'Ok') {
		return _elm_lang$core$Task$succeed(_p6._0);
	} else {
		return _elm_lang$core$Task$fail(_p6._0);
	}
};
var _elm_lang$core$Task$init = _elm_lang$core$Task$succeed(
	{ctor: '_Tuple0'});
var _elm_lang$core$Task$onSelfMsg = F3(
	function (_p9, _p8, _p7) {
		return _elm_lang$core$Task$succeed(
			{ctor: '_Tuple0'});
	});
var _elm_lang$core$Task$command = _elm_lang$core$Native_Platform.leaf('Task');
var _elm_lang$core$Task$T = function (a) {
	return {ctor: 'T', _0: a};
};
var _elm_lang$core$Task$perform = F3(
	function (onFail, onSuccess, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$T(
				A2(
					_elm_lang$core$Task$onError,
					A2(_elm_lang$core$Task$map, onSuccess, task),
					function (x) {
						return _elm_lang$core$Task$succeed(
							onFail(x));
					})));
	});
var _elm_lang$core$Task$cmdMap = F2(
	function (tagger, _p10) {
		var _p11 = _p10;
		return _elm_lang$core$Task$T(
			A2(_elm_lang$core$Task$map, tagger, _p11._0));
	});
_elm_lang$core$Native_Platform.effectManagers['Task'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Task$init, onEffects: _elm_lang$core$Task$onEffects, onSelfMsg: _elm_lang$core$Task$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Task$cmdMap};

var _elm_lang$core$Time$setInterval = _elm_lang$core$Native_Time.setInterval_;
var _elm_lang$core$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		var _p0 = intervals;
		if (_p0.ctor === '[]') {
			return _elm_lang$core$Task$succeed(processes);
		} else {
			var _p1 = _p0._0;
			return A2(
				_elm_lang$core$Task$andThen,
				_elm_lang$core$Native_Scheduler.spawn(
					A2(
						_elm_lang$core$Time$setInterval,
						_p1,
						A2(_elm_lang$core$Platform$sendToSelf, router, _p1))),
				function (id) {
					return A3(
						_elm_lang$core$Time$spawnHelp,
						router,
						_p0._1,
						A3(_elm_lang$core$Dict$insert, _p1, id, processes));
				});
		}
	});
var _elm_lang$core$Time$addMySub = F2(
	function (_p2, state) {
		var _p3 = _p2;
		var _p6 = _p3._1;
		var _p5 = _p3._0;
		var _p4 = A2(_elm_lang$core$Dict$get, _p5, state);
		if (_p4.ctor === 'Nothing') {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				_elm_lang$core$Native_List.fromArray(
					[_p6]),
				state);
		} else {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				A2(_elm_lang$core$List_ops['::'], _p6, _p4._0),
				state);
		}
	});
var _elm_lang$core$Time$inMilliseconds = function (t) {
	return t;
};
var _elm_lang$core$Time$millisecond = 1;
var _elm_lang$core$Time$second = 1000 * _elm_lang$core$Time$millisecond;
var _elm_lang$core$Time$minute = 60 * _elm_lang$core$Time$second;
var _elm_lang$core$Time$hour = 60 * _elm_lang$core$Time$minute;
var _elm_lang$core$Time$inHours = function (t) {
	return t / _elm_lang$core$Time$hour;
};
var _elm_lang$core$Time$inMinutes = function (t) {
	return t / _elm_lang$core$Time$minute;
};
var _elm_lang$core$Time$inSeconds = function (t) {
	return t / _elm_lang$core$Time$second;
};
var _elm_lang$core$Time$now = _elm_lang$core$Native_Time.now;
var _elm_lang$core$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _p7 = A2(_elm_lang$core$Dict$get, interval, state.taggers);
		if (_p7.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			return A2(
				_elm_lang$core$Task$andThen,
				_elm_lang$core$Time$now,
				function (time) {
					return A2(
						_elm_lang$core$Task$andThen,
						_elm_lang$core$Task$sequence(
							A2(
								_elm_lang$core$List$map,
								function (tagger) {
									return A2(
										_elm_lang$core$Platform$sendToApp,
										router,
										tagger(time));
								},
								_p7._0)),
						function (_p8) {
							return _elm_lang$core$Task$succeed(state);
						});
				});
		}
	});
var _elm_lang$core$Time$subscription = _elm_lang$core$Native_Platform.leaf('Time');
var _elm_lang$core$Time$State = F2(
	function (a, b) {
		return {taggers: a, processes: b};
	});
var _elm_lang$core$Time$init = _elm_lang$core$Task$succeed(
	A2(_elm_lang$core$Time$State, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty));
var _elm_lang$core$Time$onEffects = F3(
	function (router, subs, _p9) {
		var _p10 = _p9;
		var rightStep = F3(
			function (_p12, id, _p11) {
				var _p13 = _p11;
				return {
					ctor: '_Tuple3',
					_0: _p13._0,
					_1: _p13._1,
					_2: A2(
						_elm_lang$core$Task$andThen,
						_elm_lang$core$Native_Scheduler.kill(id),
						function (_p14) {
							return _p13._2;
						})
				};
			});
		var bothStep = F4(
			function (interval, taggers, id, _p15) {
				var _p16 = _p15;
				return {
					ctor: '_Tuple3',
					_0: _p16._0,
					_1: A3(_elm_lang$core$Dict$insert, interval, id, _p16._1),
					_2: _p16._2
				};
			});
		var leftStep = F3(
			function (interval, taggers, _p17) {
				var _p18 = _p17;
				return {
					ctor: '_Tuple3',
					_0: A2(_elm_lang$core$List_ops['::'], interval, _p18._0),
					_1: _p18._1,
					_2: _p18._2
				};
			});
		var newTaggers = A3(_elm_lang$core$List$foldl, _elm_lang$core$Time$addMySub, _elm_lang$core$Dict$empty, subs);
		var _p19 = A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			_p10.processes,
			{
				ctor: '_Tuple3',
				_0: _elm_lang$core$Native_List.fromArray(
					[]),
				_1: _elm_lang$core$Dict$empty,
				_2: _elm_lang$core$Task$succeed(
					{ctor: '_Tuple0'})
			});
		var spawnList = _p19._0;
		var existingDict = _p19._1;
		var killTask = _p19._2;
		return A2(
			_elm_lang$core$Task$andThen,
			killTask,
			function (_p20) {
				return A2(
					_elm_lang$core$Task$andThen,
					A3(_elm_lang$core$Time$spawnHelp, router, spawnList, existingDict),
					function (newProcesses) {
						return _elm_lang$core$Task$succeed(
							A2(_elm_lang$core$Time$State, newTaggers, newProcesses));
					});
			});
	});
var _elm_lang$core$Time$Every = F2(
	function (a, b) {
		return {ctor: 'Every', _0: a, _1: b};
	});
var _elm_lang$core$Time$every = F2(
	function (interval, tagger) {
		return _elm_lang$core$Time$subscription(
			A2(_elm_lang$core$Time$Every, interval, tagger));
	});
var _elm_lang$core$Time$subMap = F2(
	function (f, _p21) {
		var _p22 = _p21;
		return A2(
			_elm_lang$core$Time$Every,
			_p22._0,
			function (_p23) {
				return f(
					_p22._1(_p23));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Time'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Time$init, onEffects: _elm_lang$core$Time$onEffects, onSelfMsg: _elm_lang$core$Time$onSelfMsg, tag: 'sub', subMap: _elm_lang$core$Time$subMap};

var _elm_lang$core$Process$kill = _elm_lang$core$Native_Scheduler.kill;
var _elm_lang$core$Process$sleep = _elm_lang$core$Native_Scheduler.sleep;
var _elm_lang$core$Process$spawn = _elm_lang$core$Native_Scheduler.spawn;

var _debois$elm_mdl$Material_Helpers$noAttr = A2(_elm_lang$html$Html_Attributes$attribute, 'data-elm-mdl-noop', '');
var _debois$elm_mdl$Material_Helpers$aria = F2(
	function (name, value) {
		return value ? A2(
			_elm_lang$html$Html_Attributes$attribute,
			A2(_elm_lang$core$Basics_ops['++'], 'aria-', name),
			'true') : _debois$elm_mdl$Material_Helpers$noAttr;
	});
var _debois$elm_mdl$Material_Helpers$delay = F2(
	function (t, x) {
		return A3(
			_elm_lang$core$Task$perform,
			_elm_lang$core$Basics$always(x),
			_elm_lang$core$Basics$always(x),
			_elm_lang$core$Process$sleep(t));
	});
var _debois$elm_mdl$Material_Helpers$cssTransitionStep = function (x) {
	return A2(_debois$elm_mdl$Material_Helpers$delay, 50, x);
};
var _debois$elm_mdl$Material_Helpers$cmd = function (msg) {
	return A3(
		_elm_lang$core$Task$perform,
		_elm_lang$core$Basics$always(msg),
		_elm_lang$core$Basics$always(msg),
		_elm_lang$core$Task$succeed(msg));
};
var _debois$elm_mdl$Material_Helpers$lift = F6(
	function (get, set, fwd, update, action, model) {
		var _p0 = A2(
			update,
			action,
			get(model));
		var submodel$ = _p0._0;
		var e = _p0._1;
		return {
			ctor: '_Tuple2',
			_0: A2(set, model, submodel$),
			_1: A2(_elm_lang$core$Platform_Cmd$map, fwd, e)
		};
	});
var _debois$elm_mdl$Material_Helpers$lift$ = F5(
	function (get, set, update, action, model) {
		return {
			ctor: '_Tuple2',
			_0: A2(
				set,
				model,
				A2(
					update,
					action,
					get(model))),
			_1: _elm_lang$core$Platform_Cmd$none
		};
	});
var _debois$elm_mdl$Material_Helpers$map2nd = F2(
	function (f, _p1) {
		var _p2 = _p1;
		return {
			ctor: '_Tuple2',
			_0: _p2._0,
			_1: f(_p2._1)
		};
	});
var _debois$elm_mdl$Material_Helpers$map1st = F2(
	function (f, _p3) {
		var _p4 = _p3;
		return {
			ctor: '_Tuple2',
			_0: f(_p4._0),
			_1: _p4._1
		};
	});
var _debois$elm_mdl$Material_Helpers$blurOn = function (evt) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		A2(_elm_lang$core$Basics_ops['++'], 'on', evt),
		'this.blur()');
};
var _debois$elm_mdl$Material_Helpers$effect = F2(
	function (e, x) {
		return {ctor: '_Tuple2', _0: x, _1: e};
	});
var _debois$elm_mdl$Material_Helpers$pure = _debois$elm_mdl$Material_Helpers$effect(_elm_lang$core$Platform_Cmd$none);
var _debois$elm_mdl$Material_Helpers$filter = F3(
	function (elem, attr, html) {
		return A2(
			elem,
			attr,
			A2(
				_elm_lang$core$List$filterMap,
				function (x) {
					return x;
				},
				html));
	});

var _elm_lang$html$Html_Events$keyCode = A2(_elm_lang$core$Json_Decode_ops[':='], 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$html$Html_Events$targetChecked = A2(
	_elm_lang$core$Json_Decode$at,
	_elm_lang$core$Native_List.fromArray(
		['target', 'checked']),
	_elm_lang$core$Json_Decode$bool);
var _elm_lang$html$Html_Events$targetValue = A2(
	_elm_lang$core$Json_Decode$at,
	_elm_lang$core$Native_List.fromArray(
		['target', 'value']),
	_elm_lang$core$Json_Decode$string);
var _elm_lang$html$Html_Events$defaultOptions = _elm_lang$virtual_dom$VirtualDom$defaultOptions;
var _elm_lang$html$Html_Events$onWithOptions = _elm_lang$virtual_dom$VirtualDom$onWithOptions;
var _elm_lang$html$Html_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
var _elm_lang$html$Html_Events$onFocus = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'focus',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onBlur = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'blur',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onSubmitOptions = _elm_lang$core$Native_Utils.update(
	_elm_lang$html$Html_Events$defaultOptions,
	{preventDefault: true});
var _elm_lang$html$Html_Events$onSubmit = function (msg) {
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		'submit',
		_elm_lang$html$Html_Events$onSubmitOptions,
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onCheck = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'change',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetChecked));
};
var _elm_lang$html$Html_Events$onInput = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'input',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetValue));
};
var _elm_lang$html$Html_Events$onMouseOut = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseout',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseOver = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseover',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseLeave = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseleave',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseEnter = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseenter',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseUp = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseup',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseDown = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mousedown',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onDoubleClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'dblclick',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});

var _elm_lang$html$Html_App$programWithFlags = _elm_lang$virtual_dom$VirtualDom$programWithFlags;
var _elm_lang$html$Html_App$program = function (app) {
	return _elm_lang$html$Html_App$programWithFlags(
		_elm_lang$core$Native_Utils.update(
			app,
			{
				init: function (_p0) {
					return app.init;
				}
			}));
};
var _elm_lang$html$Html_App$beginnerProgram = function (_p1) {
	var _p2 = _p1;
	return _elm_lang$html$Html_App$programWithFlags(
		{
			init: function (_p3) {
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					_p2.model,
					_elm_lang$core$Native_List.fromArray(
						[]));
			},
			update: F2(
				function (msg, model) {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						A2(_p2.update, msg, model),
						_elm_lang$core$Native_List.fromArray(
							[]));
				}),
			view: _p2.view,
			subscriptions: function (_p4) {
				return _elm_lang$core$Platform_Sub$none;
			}
		});
};
var _elm_lang$html$Html_App$map = _elm_lang$virtual_dom$VirtualDom$map;

var _debois$elm_mdl$Material_Options_Internal$None = {ctor: 'None'};
var _debois$elm_mdl$Material_Options_Internal$Set = function (a) {
	return {ctor: 'Set', _0: a};
};
var _debois$elm_mdl$Material_Options_Internal$Many = function (a) {
	return {ctor: 'Many', _0: a};
};
var _debois$elm_mdl$Material_Options_Internal$Attribute = function (a) {
	return {ctor: 'Attribute', _0: a};
};
var _debois$elm_mdl$Material_Options_Internal$attribute = _debois$elm_mdl$Material_Options_Internal$Attribute;
var _debois$elm_mdl$Material_Options_Internal$CSS = function (a) {
	return {ctor: 'CSS', _0: a};
};
var _debois$elm_mdl$Material_Options_Internal$Class = function (a) {
	return {ctor: 'Class', _0: a};
};

var _debois$elm_mdl$Material_Options$id = function (_p0) {
	return _debois$elm_mdl$Material_Options_Internal$Attribute(
		_elm_lang$html$Html_Attributes$id(_p0));
};
var _debois$elm_mdl$Material_Options$attribute = _debois$elm_mdl$Material_Options_Internal$Attribute;
var _debois$elm_mdl$Material_Options$stylesheet = function (css) {
	return A3(
		_elm_lang$html$Html$node,
		'style',
		_elm_lang$core$Native_List.fromArray(
			[]),
		_elm_lang$core$Native_List.fromArray(
			[
				_elm_lang$html$Html$text(css)
			]));
};
var _debois$elm_mdl$Material_Options$data = F2(
	function (key, val) {
		return _debois$elm_mdl$Material_Options_Internal$Attribute(
			A2(
				_elm_lang$html$Html_Attributes$attribute,
				A2(_elm_lang$core$Basics_ops['++'], 'data-', key),
				val));
	});
var _debois$elm_mdl$Material_Options$set = _debois$elm_mdl$Material_Options_Internal$Set;
var _debois$elm_mdl$Material_Options$inner = function (options) {
	return _debois$elm_mdl$Material_Options$set(
		function (c) {
			return _elm_lang$core$Native_Utils.update(
				c,
				{
					inner: A2(_elm_lang$core$Basics_ops['++'], options, c.inner)
				});
		});
};
var _debois$elm_mdl$Material_Options$nop = _debois$elm_mdl$Material_Options_Internal$None;
var _debois$elm_mdl$Material_Options$when = F2(
	function (prop, guard) {
		return guard ? prop : _debois$elm_mdl$Material_Options$nop;
	});
var _debois$elm_mdl$Material_Options$maybe = function (prop) {
	return A2(_elm_lang$core$Maybe$withDefault, _debois$elm_mdl$Material_Options$nop, prop);
};
var _debois$elm_mdl$Material_Options$many = _debois$elm_mdl$Material_Options_Internal$Many;
var _debois$elm_mdl$Material_Options$css = F2(
	function (key, value) {
		return _debois$elm_mdl$Material_Options_Internal$CSS(
			{ctor: '_Tuple2', _0: key, _1: value});
	});
var _debois$elm_mdl$Material_Options$center = _debois$elm_mdl$Material_Options$many(
	_elm_lang$core$Native_List.fromArray(
		[
			A2(_debois$elm_mdl$Material_Options$css, 'display', 'flex'),
			A2(_debois$elm_mdl$Material_Options$css, 'align-items', 'center'),
			A2(_debois$elm_mdl$Material_Options$css, 'justify-content', 'center')
		]));
var _debois$elm_mdl$Material_Options$scrim = function (opacity) {
	return A2(
		_debois$elm_mdl$Material_Options$css,
		'background',
		A2(
			_elm_lang$core$Basics_ops['++'],
			'linear-gradient(rgba(0, 0, 0, 0), rgba(0, 0, 0, ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(opacity),
				'))')));
};
var _debois$elm_mdl$Material_Options$cs = function (c) {
	return _debois$elm_mdl$Material_Options_Internal$Class(c);
};
var _debois$elm_mdl$Material_Options$disabled = function (v) {
	return _debois$elm_mdl$Material_Options_Internal$Attribute(
		_elm_lang$html$Html_Attributes$disabled(v));
};
var _debois$elm_mdl$Material_Options$addAttributes = F2(
	function (summary, attrs) {
		return A2(
			_elm_lang$core$List$append,
			attrs,
			A2(
				_elm_lang$core$List_ops['::'],
				_elm_lang$html$Html_Attributes$style(summary.css),
				A2(
					_elm_lang$core$List_ops['::'],
					_elm_lang$html$Html_Attributes$class(
						A2(_elm_lang$core$String$join, ' ', summary.classes)),
					summary.attrs)));
	});
var _debois$elm_mdl$Material_Options$collect1$ = F2(
	function (options, acc) {
		var _p1 = options;
		switch (_p1.ctor) {
			case 'Class':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						classes: A2(_elm_lang$core$List_ops['::'], _p1._0, acc.classes)
					});
			case 'CSS':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						css: A2(_elm_lang$core$List_ops['::'], _p1._0, acc.css)
					});
			case 'Attribute':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						attrs: A2(_elm_lang$core$List_ops['::'], _p1._0, acc.attrs)
					});
			case 'Many':
				return A3(_elm_lang$core$List$foldl, _debois$elm_mdl$Material_Options$collect1$, acc, _p1._0);
			case 'Set':
				return acc;
			default:
				return acc;
		}
	});
var _debois$elm_mdl$Material_Options$collect1 = F2(
	function (option, acc) {
		var _p2 = option;
		switch (_p2.ctor) {
			case 'Class':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						classes: A2(_elm_lang$core$List_ops['::'], _p2._0, acc.classes)
					});
			case 'CSS':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						css: A2(_elm_lang$core$List_ops['::'], _p2._0, acc.css)
					});
			case 'Attribute':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						attrs: A2(_elm_lang$core$List_ops['::'], _p2._0, acc.attrs)
					});
			case 'Many':
				return A3(_elm_lang$core$List$foldl, _debois$elm_mdl$Material_Options$collect1, acc, _p2._0);
			case 'Set':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						config: _p2._0(acc.config)
					});
			default:
				return acc;
		}
	});
var _debois$elm_mdl$Material_Options$recollect = _elm_lang$core$List$foldl(_debois$elm_mdl$Material_Options$collect1);
var _debois$elm_mdl$Material_Options$apply = F4(
	function (summary, ctor, options, attrs) {
		return ctor(
			A2(
				_debois$elm_mdl$Material_Options$addAttributes,
				A2(_debois$elm_mdl$Material_Options$recollect, summary, options),
				attrs));
	});
var _debois$elm_mdl$Material_Options$Summary = F4(
	function (a, b, c, d) {
		return {classes: a, css: b, attrs: c, config: d};
	});
var _debois$elm_mdl$Material_Options$collect = function (_p3) {
	return _debois$elm_mdl$Material_Options$recollect(
		A4(
			_debois$elm_mdl$Material_Options$Summary,
			_elm_lang$core$Native_List.fromArray(
				[]),
			_elm_lang$core$Native_List.fromArray(
				[]),
			_elm_lang$core$Native_List.fromArray(
				[]),
			_p3));
};
var _debois$elm_mdl$Material_Options$collect$ = A2(
	_elm_lang$core$List$foldl,
	_debois$elm_mdl$Material_Options$collect1$,
	A4(
		_debois$elm_mdl$Material_Options$Summary,
		_elm_lang$core$Native_List.fromArray(
			[]),
		_elm_lang$core$Native_List.fromArray(
			[]),
		_elm_lang$core$Native_List.fromArray(
			[]),
		{ctor: '_Tuple0'}));
var _debois$elm_mdl$Material_Options$styled = F2(
	function (ctor, props) {
		return ctor(
			A2(
				_debois$elm_mdl$Material_Options$addAttributes,
				_debois$elm_mdl$Material_Options$collect$(props),
				_elm_lang$core$Native_List.fromArray(
					[])));
	});
var _debois$elm_mdl$Material_Options$div = _debois$elm_mdl$Material_Options$styled(_elm_lang$html$Html$div);
var _debois$elm_mdl$Material_Options$span = _debois$elm_mdl$Material_Options$styled(_elm_lang$html$Html$span);
var _debois$elm_mdl$Material_Options$styled$ = F3(
	function (ctor, props, attrs) {
		return ctor(
			A2(
				_debois$elm_mdl$Material_Options$addAttributes,
				_debois$elm_mdl$Material_Options$collect$(props),
				attrs));
	});
var _debois$elm_mdl$Material_Options$img = F2(
	function (options, attrs) {
		return A4(
			_debois$elm_mdl$Material_Options$styled$,
			_elm_lang$html$Html$img,
			options,
			attrs,
			_elm_lang$core$Native_List.fromArray(
				[]));
	});

var _debois$elm_mdl$Material_Ripple$styles = F2(
	function (m, frame) {
		var r = m.rect;
		var toPx = function (k) {
			return A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(
					_elm_lang$core$Basics$round(k)),
				'px');
		};
		var offset = A2(
			_elm_lang$core$Basics_ops['++'],
			'translate(',
			A2(
				_elm_lang$core$Basics_ops['++'],
				toPx(m.x),
				A2(
					_elm_lang$core$Basics_ops['++'],
					', ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						toPx(m.y),
						')'))));
		var rippleSize = toPx(
			(_elm_lang$core$Basics$sqrt((r.width * r.width) + (r.height * r.height)) * 2.0) + 2.0);
		var scale = _elm_lang$core$Native_Utils.eq(frame, 0) ? 'scale(0.0001, 0.0001)' : '';
		var transformString = A2(
			_elm_lang$core$Basics_ops['++'],
			'translate(-50%, -50%) ',
			A2(_elm_lang$core$Basics_ops['++'], offset, scale));
		return _elm_lang$core$Native_List.fromArray(
			[
				{ctor: '_Tuple2', _0: 'width', _1: rippleSize},
				{ctor: '_Tuple2', _0: 'height', _1: rippleSize},
				{ctor: '_Tuple2', _0: '-webkit-transform', _1: transformString},
				{ctor: '_Tuple2', _0: '-ms-transform', _1: transformString},
				{ctor: '_Tuple2', _0: 'transform', _1: transformString}
			]);
	});
var _debois$elm_mdl$Material_Ripple$Metrics = F3(
	function (a, b, c) {
		return {rect: a, x: b, y: c};
	});
var _debois$elm_mdl$Material_Ripple$computeMetrics = function (g) {
	var rect = g.rect;
	var set = F2(
		function (x, y) {
			return _elm_lang$core$Maybe$Just(
				{ctor: '_Tuple2', _0: x - rect.left, _1: y - rect.top});
		});
	return A2(
		_elm_lang$core$Maybe$map,
		function (_p0) {
			var _p1 = _p0;
			return A3(_debois$elm_mdl$Material_Ripple$Metrics, rect, _p1._0, _p1._1);
		},
		function () {
			var _p2 = {ctor: '_Tuple4', _0: g.clientX, _1: g.clientY, _2: g.touchX, _3: g.touchY};
			_v1_3:
			do {
				if (_p2.ctor === '_Tuple4') {
					if ((_p2._0.ctor === 'Just') && (_p2._1.ctor === 'Just')) {
						if ((_p2._0._0 === 0.0) && (_p2._1._0 === 0.0)) {
							return _elm_lang$core$Maybe$Just(
								{ctor: '_Tuple2', _0: rect.width / 2.0, _1: rect.height / 2.0});
						} else {
							return A2(set, _p2._0._0, _p2._1._0);
						}
					} else {
						if ((_p2._2.ctor === 'Just') && (_p2._3.ctor === 'Just')) {
							return A2(set, _p2._2._0, _p2._3._0);
						} else {
							break _v1_3;
						}
					}
				} else {
					break _v1_3;
				}
			} while(false);
			return _elm_lang$core$Maybe$Nothing;
		}());
};
var _debois$elm_mdl$Material_Ripple$Model = F3(
	function (a, b, c) {
		return {animation: a, metrics: b, ignoringMouseDown: c};
	});
var _debois$elm_mdl$Material_Ripple$DOMState = F6(
	function (a, b, c, d, e, f) {
		return {rect: a, clientX: b, clientY: c, touchX: d, touchY: e, type$: f};
	});
var _debois$elm_mdl$Material_Ripple$geometryDecoder = A7(
	_elm_lang$core$Json_Decode$object6,
	_debois$elm_mdl$Material_Ripple$DOMState,
	A2(_elm_lang$core$Json_Decode_ops[':='], 'currentTarget', _debois$elm_dom$DOM$boundingClientRect),
	_elm_lang$core$Json_Decode$maybe(
		A2(_elm_lang$core$Json_Decode_ops[':='], 'clientX', _elm_lang$core$Json_Decode$float)),
	_elm_lang$core$Json_Decode$maybe(
		A2(_elm_lang$core$Json_Decode_ops[':='], 'clientY', _elm_lang$core$Json_Decode$float)),
	_elm_lang$core$Json_Decode$maybe(
		A2(
			_elm_lang$core$Json_Decode$at,
			_elm_lang$core$Native_List.fromArray(
				['touches', '0', 'clientX']),
			_elm_lang$core$Json_Decode$float)),
	_elm_lang$core$Json_Decode$maybe(
		A2(
			_elm_lang$core$Json_Decode$at,
			_elm_lang$core$Native_List.fromArray(
				['touches', '0', 'clientY']),
			_elm_lang$core$Json_Decode$float)),
	A2(_elm_lang$core$Json_Decode_ops[':='], 'type', _elm_lang$core$Json_Decode$string));
var _debois$elm_mdl$Material_Ripple$Inert = {ctor: 'Inert'};
var _debois$elm_mdl$Material_Ripple$model = {animation: _debois$elm_mdl$Material_Ripple$Inert, metrics: _elm_lang$core$Maybe$Nothing, ignoringMouseDown: false};
var _debois$elm_mdl$Material_Ripple$Frame = function (a) {
	return {ctor: 'Frame', _0: a};
};
var _debois$elm_mdl$Material_Ripple$view$ = F2(
	function (attrs, model) {
		var styling = function () {
			var _p3 = {ctor: '_Tuple2', _0: model.metrics, _1: model.animation};
			if ((_p3.ctor === '_Tuple2') && (_p3._0.ctor === 'Just')) {
				if (_p3._1.ctor === 'Frame') {
					return A2(_debois$elm_mdl$Material_Ripple$styles, _p3._0._0, _p3._1._0);
				} else {
					return A2(_debois$elm_mdl$Material_Ripple$styles, _p3._0._0, 1);
				}
			} else {
				return _elm_lang$core$Native_List.fromArray(
					[]);
			}
		}();
		return A2(
			_elm_lang$html$Html$span,
			attrs,
			_elm_lang$core$Native_List.fromArray(
				[
					A2(
					_elm_lang$html$Html$span,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$classList(
							_elm_lang$core$Native_List.fromArray(
								[
									{ctor: '_Tuple2', _0: 'mdl-ripple', _1: true},
									{
									ctor: '_Tuple2',
									_0: 'is-animating',
									_1: !_elm_lang$core$Native_Utils.eq(
										model.animation,
										_debois$elm_mdl$Material_Ripple$Frame(0))
								},
									{
									ctor: '_Tuple2',
									_0: 'is-visible',
									_1: !_elm_lang$core$Native_Utils.eq(model.animation, _debois$elm_mdl$Material_Ripple$Inert)
								}
								])),
							_elm_lang$html$Html_Attributes$style(styling)
						]),
					_elm_lang$core$Native_List.fromArray(
						[]))
				]));
	});
var _debois$elm_mdl$Material_Ripple$Tick = {ctor: 'Tick'};
var _debois$elm_mdl$Material_Ripple$update = F2(
	function (action, model) {
		var _p4 = action;
		switch (_p4.ctor) {
			case 'Down':
				var _p5 = _p4._0;
				return (_elm_lang$core$Native_Utils.eq(_p5.type$, 'mousedown') && model.ignoringMouseDown) ? _debois$elm_mdl$Material_Helpers$pure(
					_elm_lang$core$Native_Utils.update(
						model,
						{ignoringMouseDown: false})) : A2(
					_debois$elm_mdl$Material_Helpers$effect,
					_debois$elm_mdl$Material_Helpers$cssTransitionStep(_debois$elm_mdl$Material_Ripple$Tick),
					_elm_lang$core$Native_Utils.update(
						model,
						{
							animation: _debois$elm_mdl$Material_Ripple$Frame(0),
							metrics: _debois$elm_mdl$Material_Ripple$computeMetrics(_p5),
							ignoringMouseDown: _elm_lang$core$Native_Utils.eq(_p5.type$, 'touchstart') ? true : model.ignoringMouseDown
						}));
			case 'Up':
				return _debois$elm_mdl$Material_Helpers$pure(
					_elm_lang$core$Native_Utils.update(
						model,
						{animation: _debois$elm_mdl$Material_Ripple$Inert}));
			default:
				return _elm_lang$core$Native_Utils.eq(
					model.animation,
					_debois$elm_mdl$Material_Ripple$Frame(0)) ? _debois$elm_mdl$Material_Helpers$pure(
					_elm_lang$core$Native_Utils.update(
						model,
						{
							animation: _debois$elm_mdl$Material_Ripple$Frame(1)
						})) : _debois$elm_mdl$Material_Helpers$pure(model);
		}
	});
var _debois$elm_mdl$Material_Ripple$Up = {ctor: 'Up'};
var _debois$elm_mdl$Material_Ripple$upOn$ = F2(
	function (f, name) {
		return A2(
			_elm_lang$html$Html_Events$on,
			name,
			_elm_lang$core$Json_Decode$succeed(
				f(_debois$elm_mdl$Material_Ripple$Up)));
	});
var _debois$elm_mdl$Material_Ripple$upOn = _debois$elm_mdl$Material_Ripple$upOn$(_elm_lang$core$Basics$identity);
var _debois$elm_mdl$Material_Ripple$Down = function (a) {
	return {ctor: 'Down', _0: a};
};
var _debois$elm_mdl$Material_Ripple$downOn$ = F2(
	function (f, name) {
		return A2(
			_elm_lang$html$Html_Events$on,
			name,
			A2(
				_elm_lang$core$Json_Decode$map,
				function (_p6) {
					return f(
						_debois$elm_mdl$Material_Ripple$Down(_p6));
				},
				_debois$elm_mdl$Material_Ripple$geometryDecoder));
	});
var _debois$elm_mdl$Material_Ripple$downOn = _debois$elm_mdl$Material_Ripple$downOn$(_elm_lang$core$Basics$identity);
var _debois$elm_mdl$Material_Ripple$view = function (_p7) {
	return _debois$elm_mdl$Material_Ripple$view$(
		A3(
			_elm_lang$core$Basics$flip,
			_elm_lang$core$List$append,
			_elm_lang$core$Native_List.fromArray(
				[
					_debois$elm_mdl$Material_Ripple$upOn('mouseup'),
					_debois$elm_mdl$Material_Ripple$upOn('mouseleave'),
					_debois$elm_mdl$Material_Ripple$upOn('touchend'),
					_debois$elm_mdl$Material_Ripple$upOn('blur'),
					_debois$elm_mdl$Material_Ripple$downOn('mousedown'),
					_debois$elm_mdl$Material_Ripple$downOn('touchstart')
				]),
			_p7));
};

var _debois$elm_mdl$Material_Button$icon = _debois$elm_mdl$Material_Options$cs('mdl-button--icon');
var _debois$elm_mdl$Material_Button$minifab = _debois$elm_mdl$Material_Options$cs('mdl-button--mini-fab');
var _debois$elm_mdl$Material_Button$fab = _debois$elm_mdl$Material_Options$cs('mdl-button--fab');
var _debois$elm_mdl$Material_Button$raised = _debois$elm_mdl$Material_Options$cs('mdl-button--raised');
var _debois$elm_mdl$Material_Button$flat = _debois$elm_mdl$Material_Options$nop;
var _debois$elm_mdl$Material_Button$blurAndForward = function (event) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		A2(_elm_lang$core$Basics_ops['++'], 'on', event),
		'this.blur(); (function(self) { var e = document.createEvent(\'Event\'); e.initEvent(\'touchcancel\', true, true); self.lastChild.dispatchEvent(e); }(this));');
};
var _debois$elm_mdl$Material_Button$type$ = function (tp) {
	return _debois$elm_mdl$Material_Options$set(
		function (options) {
			return _elm_lang$core$Native_Utils.update(
				options,
				{
					type$: _elm_lang$core$Maybe$Just(tp)
				});
		});
};
var _debois$elm_mdl$Material_Button$accent = _debois$elm_mdl$Material_Options$cs('mdl-button--accent');
var _debois$elm_mdl$Material_Button$primary = _debois$elm_mdl$Material_Options$cs('mdl-button--primary');
var _debois$elm_mdl$Material_Button$colored = _debois$elm_mdl$Material_Options$cs('mdl-button--colored');
var _debois$elm_mdl$Material_Button$plain = _debois$elm_mdl$Material_Options$nop;
var _debois$elm_mdl$Material_Button$disabled = _debois$elm_mdl$Material_Options$set(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{disabled: true});
	});
var _debois$elm_mdl$Material_Button$ripple = _debois$elm_mdl$Material_Options$set(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{ripple: true});
	});
var _debois$elm_mdl$Material_Button$onClick = function (x) {
	return _debois$elm_mdl$Material_Options$set(
		function (options) {
			return _elm_lang$core$Native_Utils.update(
				options,
				{
					onClick: _elm_lang$core$Maybe$Just(
						_elm_lang$html$Html_Events$onClick(x))
				});
		});
};
var _debois$elm_mdl$Material_Button$defaultConfig = {ripple: false, onClick: _elm_lang$core$Maybe$Nothing, disabled: false, type$: _elm_lang$core$Maybe$Nothing};
var _debois$elm_mdl$Material_Button$view = F4(
	function (lift, model, config, html) {
		var summary = A2(_debois$elm_mdl$Material_Options$collect, _debois$elm_mdl$Material_Button$defaultConfig, config);
		var startListeners = summary.config.ripple ? _elm_lang$core$Native_List.fromArray(
			[
				_elm_lang$core$Maybe$Just(
				A2(_debois$elm_mdl$Material_Ripple$downOn$, lift, 'mousedown')),
				_elm_lang$core$Maybe$Just(
				A2(_debois$elm_mdl$Material_Ripple$downOn$, lift, 'touchstart'))
			]) : _elm_lang$core$Native_List.fromArray(
			[]);
		var stopListeners = function () {
			var handle = function (_p0) {
				return _elm_lang$core$Maybe$Just(
					(summary.config.ripple ? _debois$elm_mdl$Material_Button$blurAndForward : _debois$elm_mdl$Material_Helpers$blurOn)(_p0));
			};
			return _elm_lang$core$Native_List.fromArray(
				[
					handle('mouseup'),
					handle('mouseleave'),
					handle('touchend')
				]);
		}();
		var misc = _elm_lang$core$Native_List.fromArray(
			[
				summary.config.onClick,
				summary.config.disabled ? _elm_lang$core$Maybe$Just(
				_elm_lang$html$Html_Attributes$disabled(true)) : _elm_lang$core$Maybe$Nothing
			]);
		var type$ = function () {
			var _p1 = summary.config.type$;
			if (_p1.ctor === 'Nothing') {
				return _elm_lang$core$Native_List.fromArray(
					[]);
			} else {
				return _elm_lang$core$Native_List.fromArray(
					[
						_elm_lang$core$Maybe$Just(
						_elm_lang$html$Html_Attributes$type$(_p1._0))
					]);
			}
		}();
		return A5(
			_debois$elm_mdl$Material_Options$apply,
			summary,
			_elm_lang$html$Html$button,
			_elm_lang$core$Native_List.fromArray(
				[
					_debois$elm_mdl$Material_Options$cs('mdl-button'),
					_debois$elm_mdl$Material_Options$cs('mdl-js-button'),
					A2(
					_debois$elm_mdl$Material_Options$when,
					_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect'),
					summary.config.ripple)
				]),
			A2(
				_elm_lang$core$List$filterMap,
				_elm_lang$core$Basics$identity,
				_elm_lang$core$List$concat(
					_elm_lang$core$Native_List.fromArray(
						[startListeners, stopListeners, misc, type$]))),
			summary.config.ripple ? _elm_lang$core$List$concat(
				_elm_lang$core$Native_List.fromArray(
					[
						html,
						_elm_lang$core$Native_List.fromArray(
						[
							A2(
							_elm_lang$html$Html_App$map,
							lift,
							A2(
								_debois$elm_mdl$Material_Ripple$view$,
								_elm_lang$core$Native_List.fromArray(
									[
										_elm_lang$html$Html_Attributes$class('mdl-button__ripple-container'),
										_debois$elm_mdl$Material_Ripple$upOn('blur'),
										_debois$elm_mdl$Material_Ripple$upOn('touchcancel')
									]),
								model))
						])
					])) : html);
	});
var _debois$elm_mdl$Material_Button$update = function (action) {
	return _debois$elm_mdl$Material_Ripple$update(action);
};
var _debois$elm_mdl$Material_Button$render = A5(
	_debois$elm_parts$Parts$create,
	_debois$elm_mdl$Material_Button$view,
	_debois$elm_parts$Parts$generalize(_debois$elm_mdl$Material_Button$update),
	function (_) {
		return _.button;
	},
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.update(
				y,
				{button: x});
		}),
	_debois$elm_mdl$Material_Ripple$model);
var _debois$elm_mdl$Material_Button$defaultModel = _debois$elm_mdl$Material_Ripple$model;
var _debois$elm_mdl$Material_Button$Config = F4(
	function (a, b, c, d) {
		return {ripple: a, onClick: b, disabled: c, type$: d};
	});

var _debois$elm_mdl$Material_Textfield$update = F2(
	function (action, model) {
		var _p0 = action;
		switch (_p0.ctor) {
			case 'Input':
				return _elm_lang$core$Native_Utils.update(
					model,
					{value: _p0._0});
			case 'Blur':
				return _elm_lang$core$Native_Utils.update(
					model,
					{isFocused: false});
			default:
				return _elm_lang$core$Native_Utils.update(
					model,
					{isFocused: true});
		}
	});
var _debois$elm_mdl$Material_Textfield$defaultModel = {isFocused: false, value: ''};
var _debois$elm_mdl$Material_Textfield$cols = function (cols) {
	return _debois$elm_mdl$Material_Options$set(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{
					cols: _elm_lang$core$Maybe$Just(cols)
				});
		});
};
var _debois$elm_mdl$Material_Textfield$rows = function (rows) {
	return _debois$elm_mdl$Material_Options$set(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{
					rows: _elm_lang$core$Maybe$Just(rows)
				});
		});
};
var _debois$elm_mdl$Material_Textfield$style = _debois$elm_mdl$Material_Options$inner;
var _debois$elm_mdl$Material_Textfield$on = F2(
	function (event, decoder) {
		return _debois$elm_mdl$Material_Options$set(
			function (config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						listeners: A2(
							_elm_lang$core$Basics_ops['++'],
							config.listeners,
							_elm_lang$core$Native_List.fromArray(
								[
									A2(_elm_lang$html$Html_Events$on, event, decoder)
								]))
					});
			});
	});
var _debois$elm_mdl$Material_Textfield$onInput = function (f) {
	return A2(
		_debois$elm_mdl$Material_Textfield$on,
		'input',
		A2(_elm_lang$core$Json_Decode$map, f, _elm_lang$html$Html_Events$targetValue));
};
var _debois$elm_mdl$Material_Textfield$onBlur = function (f) {
	return A2(
		_debois$elm_mdl$Material_Textfield$on,
		'focusout',
		_elm_lang$core$Json_Decode$succeed(f));
};
var _debois$elm_mdl$Material_Textfield$onFocus = function (f) {
	return A2(
		_debois$elm_mdl$Material_Textfield$on,
		'focusin',
		_elm_lang$core$Json_Decode$succeed(f));
};
var _debois$elm_mdl$Material_Textfield$disabled = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{disabled: true});
	});
var _debois$elm_mdl$Material_Textfield$maxlength = function (v) {
	return _debois$elm_mdl$Material_Options$set(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{
					maxlength: _elm_lang$core$Maybe$Just(v)
				});
		});
};
var _debois$elm_mdl$Material_Textfield$autofocus = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{autofocus: true});
	});
var _debois$elm_mdl$Material_Textfield$value = function (str) {
	return _debois$elm_mdl$Material_Options$set(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{
					value: _elm_lang$core$Maybe$Just(str)
				});
		});
};
var _debois$elm_mdl$Material_Textfield$error = function (str) {
	return _debois$elm_mdl$Material_Options$set(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{
					error: _elm_lang$core$Maybe$Just(str)
				});
		});
};
var _debois$elm_mdl$Material_Textfield$floatingLabel = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{labelFloat: true});
	});
var _debois$elm_mdl$Material_Textfield$label = function (str) {
	return _debois$elm_mdl$Material_Options$set(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{
					labelText: _elm_lang$core$Maybe$Just(str)
				});
		});
};
var _debois$elm_mdl$Material_Textfield$Config = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return function (l) {
												return {labelText: a, labelFloat: b, error: c, value: d, disabled: e, kind: f, rows: g, cols: h, autofocus: i, maxlength: j, inner: k, listeners: l};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _debois$elm_mdl$Material_Textfield$Model = F2(
	function (a, b) {
		return {isFocused: a, value: b};
	});
var _debois$elm_mdl$Material_Textfield$Password = {ctor: 'Password'};
var _debois$elm_mdl$Material_Textfield$password = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{kind: _debois$elm_mdl$Material_Textfield$Password});
	});
var _debois$elm_mdl$Material_Textfield$Textarea = {ctor: 'Textarea'};
var _debois$elm_mdl$Material_Textfield$textarea = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{kind: _debois$elm_mdl$Material_Textfield$Textarea});
	});
var _debois$elm_mdl$Material_Textfield$Text = {ctor: 'Text'};
var _debois$elm_mdl$Material_Textfield$defaultConfig = {
	labelText: _elm_lang$core$Maybe$Nothing,
	labelFloat: false,
	error: _elm_lang$core$Maybe$Nothing,
	value: _elm_lang$core$Maybe$Nothing,
	disabled: false,
	kind: _debois$elm_mdl$Material_Textfield$Text,
	rows: _elm_lang$core$Maybe$Nothing,
	cols: _elm_lang$core$Maybe$Nothing,
	autofocus: false,
	maxlength: _elm_lang$core$Maybe$Nothing,
	inner: _elm_lang$core$Native_List.fromArray(
		[]),
	listeners: _elm_lang$core$Native_List.fromArray(
		[])
};
var _debois$elm_mdl$Material_Textfield$text$ = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{kind: _debois$elm_mdl$Material_Textfield$Text});
	});
var _debois$elm_mdl$Material_Textfield$Input = function (a) {
	return {ctor: 'Input', _0: a};
};
var _debois$elm_mdl$Material_Textfield$Focus = {ctor: 'Focus'};
var _debois$elm_mdl$Material_Textfield$Blur = {ctor: 'Blur'};
var _debois$elm_mdl$Material_Textfield$view = F3(
	function (lift, model, options) {
		var _p1 = A2(_debois$elm_mdl$Material_Options$collect, _debois$elm_mdl$Material_Textfield$defaultConfig, options);
		var summary = _p1;
		var config = _p1.config;
		var val = A2(_elm_lang$core$Maybe$withDefault, model.value, config.value);
		var isTextarea = _elm_lang$core$Native_Utils.eq(config.kind, _debois$elm_mdl$Material_Textfield$Textarea);
		var elementFunction = isTextarea ? _elm_lang$html$Html$textarea : _elm_lang$html$Html$input;
		var typeAttributes = function () {
			var _p2 = config.kind;
			switch (_p2.ctor) {
				case 'Text':
					return _elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$type$('text')
						]);
				case 'Password':
					return _elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$type$('password')
						]);
				default:
					return A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Native_List.fromArray(
							[]),
						A2(
							_elm_lang$core$Basics_ops['++'],
							function () {
								var _p3 = config.rows;
								if (_p3.ctor === 'Just') {
									return _elm_lang$core$Native_List.fromArray(
										[
											_elm_lang$html$Html_Attributes$rows(_p3._0)
										]);
								} else {
									return _elm_lang$core$Native_List.fromArray(
										[]);
								}
							}(),
							function () {
								var _p4 = config.cols;
								if (_p4.ctor === 'Just') {
									return _elm_lang$core$Native_List.fromArray(
										[
											_elm_lang$html$Html_Attributes$cols(_p4._0)
										]);
								} else {
									return _elm_lang$core$Native_List.fromArray(
										[]);
								}
							}()));
			}
		}();
		var maxlength = function () {
			var _p5 = config.maxlength;
			if (_p5.ctor === 'Just') {
				return _elm_lang$core$Native_List.fromArray(
					[
						_elm_lang$html$Html_Attributes$maxlength(_p5._0)
					]);
			} else {
				return _elm_lang$core$Native_List.fromArray(
					[]);
			}
		}();
		var listeners = config.listeners;
		var textValue = function () {
			var _p6 = config.value;
			if (_p6.ctor === 'Just') {
				return _elm_lang$core$Native_List.fromArray(
					[
						_elm_lang$html$Html_Attributes$value(_p6._0)
					]);
			} else {
				return _elm_lang$core$Native_List.fromArray(
					[]);
			}
		}();
		var defaultInput = function () {
			var _p7 = config.value;
			if (_p7.ctor === 'Just') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return _elm_lang$core$Maybe$Just(
					A2(
						_elm_lang$html$Html_Events$on,
						'input',
						A2(
							_elm_lang$core$Json_Decode$map,
							function (_p8) {
								return lift(
									_debois$elm_mdl$Material_Textfield$Input(_p8));
							},
							_elm_lang$html$Html_Events$targetValue)));
			}
		}();
		return A5(
			_debois$elm_mdl$Material_Options$apply,
			summary,
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[
					_debois$elm_mdl$Material_Options$cs('mdl-textfield'),
					_debois$elm_mdl$Material_Options$cs('mdl-js-textfield'),
					_debois$elm_mdl$Material_Options$cs('is-upgraded'),
					config.labelFloat ? _debois$elm_mdl$Material_Options$cs('mdl-textfield--floating-label') : _debois$elm_mdl$Material_Options$nop,
					(!_elm_lang$core$Native_Utils.eq(config.error, _elm_lang$core$Maybe$Nothing)) ? _debois$elm_mdl$Material_Options$cs('is-invalid') : _debois$elm_mdl$Material_Options$nop,
					(!_elm_lang$core$Native_Utils.eq(val, '')) ? _debois$elm_mdl$Material_Options$cs('is-dirty') : _debois$elm_mdl$Material_Options$nop,
					(model.isFocused && _elm_lang$core$Basics$not(config.disabled)) ? _debois$elm_mdl$Material_Options$cs('is-focused') : _debois$elm_mdl$Material_Options$nop,
					config.disabled ? _debois$elm_mdl$Material_Options$cs('is-disabled') : _debois$elm_mdl$Material_Options$nop
				]),
			A2(
				_elm_lang$core$List$filterMap,
				_elm_lang$core$Basics$identity,
				_elm_lang$core$Native_List.fromArray(
					[defaultInput])),
			_elm_lang$core$Native_List.fromArray(
				[
					A4(
					_debois$elm_mdl$Material_Options$styled$,
					elementFunction,
					_elm_lang$core$Native_List.fromArray(
						[
							_debois$elm_mdl$Material_Options$cs('mdl-textfield__input'),
							A2(_debois$elm_mdl$Material_Options$css, 'outline', 'none'),
							_debois$elm_mdl$Material_Options_Internal$attribute(
							A2(
								_elm_lang$html$Html_Events$on,
								'focus',
								_elm_lang$core$Json_Decode$succeed(
									lift(_debois$elm_mdl$Material_Textfield$Focus)))),
							_debois$elm_mdl$Material_Options_Internal$attribute(
							A2(
								_elm_lang$html$Html_Events$on,
								'blur',
								_elm_lang$core$Json_Decode$succeed(
									lift(_debois$elm_mdl$Material_Textfield$Blur)))),
							_debois$elm_mdl$Material_Options$many(config.inner)
						]),
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$html$Html_Attributes$disabled(config.disabled),
								_elm_lang$html$Html_Attributes$autofocus(config.autofocus)
							]),
						A2(
							_elm_lang$core$Basics_ops['++'],
							textValue,
							A2(
								_elm_lang$core$Basics_ops['++'],
								typeAttributes,
								A2(_elm_lang$core$Basics_ops['++'], maxlength, listeners)))),
					_elm_lang$core$Native_List.fromArray(
						[])),
					A2(
					_elm_lang$html$Html$label,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('mdl-textfield__label')
						]),
					function () {
						var _p9 = config.labelText;
						if (_p9.ctor === 'Just') {
							return _elm_lang$core$Native_List.fromArray(
								[
									_elm_lang$html$Html$text(_p9._0)
								]);
						} else {
							return _elm_lang$core$Native_List.fromArray(
								[]);
						}
					}()),
					A2(
					_elm_lang$core$Maybe$withDefault,
					A2(
						_elm_lang$html$Html$div,
						_elm_lang$core$Native_List.fromArray(
							[]),
						_elm_lang$core$Native_List.fromArray(
							[])),
					A2(
						_elm_lang$core$Maybe$map,
						function (e) {
							return A2(
								_elm_lang$html$Html$span,
								_elm_lang$core$Native_List.fromArray(
									[
										_elm_lang$html$Html_Attributes$class('mdl-textfield__error')
									]),
								_elm_lang$core$Native_List.fromArray(
									[
										_elm_lang$html$Html$text(e)
									]));
						},
						config.error))
				]));
	});
var _debois$elm_mdl$Material_Textfield$render = A5(
	_debois$elm_parts$Parts$create,
	_debois$elm_mdl$Material_Textfield$view,
	F3(
		function (_p10, msg, model) {
			return _elm_lang$core$Maybe$Just(
				{
					ctor: '_Tuple2',
					_0: A2(_debois$elm_mdl$Material_Textfield$update, msg, model),
					_1: _elm_lang$core$Platform_Cmd$none
				});
		}),
	function (_) {
		return _.textfield;
	},
	F2(
		function (x, c) {
			return _elm_lang$core$Native_Utils.update(
				c,
				{textfield: x});
		}),
	_debois$elm_mdl$Material_Textfield$defaultModel);

var _elm_lang$dom$Native_Dom = function() {

function on(node)
{
	return function(eventName, decoder, toTask)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {

			function performTask(event)
			{
				var result = A2(_elm_lang$core$Json_Decode$decodeValue, decoder, event);
				if (result.ctor === 'Ok')
				{
					_elm_lang$core$Native_Scheduler.rawSpawn(toTask(result._0));
				}
			}

			node.addEventListener(eventName, performTask);

			return function()
			{
				node.removeEventListener(eventName, performTask);
			};
		});
	};
}

var rAF = typeof requestAnimationFrame !== 'undefined'
	? requestAnimationFrame
	: function(callback) { callback(); };

function withNode(id, doStuff)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		rAF(function()
		{
			var node = document.getElementById(id);
			if (node === null)
			{
				callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NotFound', _0: id }));
				return;
			}
			callback(_elm_lang$core$Native_Scheduler.succeed(doStuff(node)));
		});
	});
}


// FOCUS

function focus(id)
{
	return withNode(id, function(node) {
		node.focus();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function blur(id)
{
	return withNode(id, function(node) {
		node.blur();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SCROLLING

function getScrollTop(id)
{
	return withNode(id, function(node) {
		return node.scrollTop;
	});
}

function setScrollTop(id, desiredScrollTop)
{
	return withNode(id, function(node) {
		node.scrollTop = desiredScrollTop;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toBottom(id)
{
	return withNode(id, function(node) {
		node.scrollTop = node.scrollHeight;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function getScrollLeft(id)
{
	return withNode(id, function(node) {
		return node.scrollLeft;
	});
}

function setScrollLeft(id, desiredScrollLeft)
{
	return withNode(id, function(node) {
		node.scrollLeft = desiredScrollLeft;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toRight(id)
{
	return withNode(id, function(node) {
		node.scrollLeft = node.scrollWidth;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SIZE

function width(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollWidth;
			case 'VisibleContent':
				return node.clientWidth;
			case 'VisibleContentWithBorders':
				return node.offsetWidth;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.right - rect.left;
		}
	});
}

function height(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollHeight;
			case 'VisibleContent':
				return node.clientHeight;
			case 'VisibleContentWithBorders':
				return node.offsetHeight;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.bottom - rect.top;
		}
	});
}

return {
	onDocument: F3(on(document)),
	onWindow: F3(on(window)),

	focus: focus,
	blur: blur,

	getScrollTop: getScrollTop,
	setScrollTop: F2(setScrollTop),
	getScrollLeft: getScrollLeft,
	setScrollLeft: F2(setScrollLeft),
	toBottom: toBottom,
	toRight: toRight,

	height: F2(height),
	width: F2(width)
};

}();

var _elm_lang$dom$Dom_LowLevel$onWindow = _elm_lang$dom$Native_Dom.onWindow;
var _elm_lang$dom$Dom_LowLevel$onDocument = _elm_lang$dom$Native_Dom.onDocument;

var _elm_lang$mouse$Mouse$onSelfMsg = F3(
	function (router, _p0, state) {
		var _p1 = _p0;
		var _p2 = A2(_elm_lang$core$Dict$get, _p1.category, state);
		if (_p2.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (tagger) {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					tagger(_p1.position));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p2._0.taggers)),
				function (_p3) {
					return _elm_lang$core$Task$succeed(state);
				});
		}
	});
var _elm_lang$mouse$Mouse_ops = _elm_lang$mouse$Mouse_ops || {};
_elm_lang$mouse$Mouse_ops['&>'] = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Task$andThen,
			t1,
			function (_p4) {
				return t2;
			});
	});
var _elm_lang$mouse$Mouse$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
var _elm_lang$mouse$Mouse$categorizeHelpHelp = F2(
	function (value, maybeValues) {
		var _p5 = maybeValues;
		if (_p5.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_List.fromArray(
					[value]));
		} else {
			return _elm_lang$core$Maybe$Just(
				A2(_elm_lang$core$List_ops['::'], value, _p5._0));
		}
	});
var _elm_lang$mouse$Mouse$categorizeHelp = F2(
	function (subs, subDict) {
		categorizeHelp:
		while (true) {
			var _p6 = subs;
			if (_p6.ctor === '[]') {
				return subDict;
			} else {
				var _v4 = _p6._1,
					_v5 = A3(
					_elm_lang$core$Dict$update,
					_p6._0._0,
					_elm_lang$mouse$Mouse$categorizeHelpHelp(_p6._0._1),
					subDict);
				subs = _v4;
				subDict = _v5;
				continue categorizeHelp;
			}
		}
	});
var _elm_lang$mouse$Mouse$categorize = function (subs) {
	return A2(_elm_lang$mouse$Mouse$categorizeHelp, subs, _elm_lang$core$Dict$empty);
};
var _elm_lang$mouse$Mouse$subscription = _elm_lang$core$Native_Platform.leaf('Mouse');
var _elm_lang$mouse$Mouse$Position = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _elm_lang$mouse$Mouse$position = A3(
	_elm_lang$core$Json_Decode$object2,
	_elm_lang$mouse$Mouse$Position,
	A2(_elm_lang$core$Json_Decode_ops[':='], 'pageX', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode_ops[':='], 'pageY', _elm_lang$core$Json_Decode$int));
var _elm_lang$mouse$Mouse$Watcher = F2(
	function (a, b) {
		return {taggers: a, pid: b};
	});
var _elm_lang$mouse$Mouse$Msg = F2(
	function (a, b) {
		return {category: a, position: b};
	});
var _elm_lang$mouse$Mouse$onEffects = F3(
	function (router, newSubs, oldState) {
		var rightStep = F3(
			function (category, taggers, task) {
				return A2(
					_elm_lang$core$Task$andThen,
					task,
					function (state) {
						return A2(
							_elm_lang$core$Task$andThen,
							_elm_lang$core$Process$spawn(
								A3(
									_elm_lang$dom$Dom_LowLevel$onDocument,
									category,
									_elm_lang$mouse$Mouse$position,
									function (_p7) {
										return A2(
											_elm_lang$core$Platform$sendToSelf,
											router,
											A2(_elm_lang$mouse$Mouse$Msg, category, _p7));
									})),
							function (pid) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$core$Dict$insert,
										category,
										A2(_elm_lang$mouse$Mouse$Watcher, taggers, pid),
										state));
							});
					});
			});
		var bothStep = F4(
			function (category, _p8, taggers, task) {
				var _p9 = _p8;
				return A2(
					_elm_lang$core$Task$andThen,
					task,
					function (state) {
						return _elm_lang$core$Task$succeed(
							A3(
								_elm_lang$core$Dict$insert,
								category,
								A2(_elm_lang$mouse$Mouse$Watcher, taggers, _p9.pid),
								state));
					});
			});
		var leftStep = F3(
			function (category, _p10, task) {
				var _p11 = _p10;
				return A2(
					_elm_lang$mouse$Mouse_ops['&>'],
					_elm_lang$core$Process$kill(_p11.pid),
					task);
			});
		return A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			oldState,
			_elm_lang$mouse$Mouse$categorize(newSubs),
			_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
	});
var _elm_lang$mouse$Mouse$MySub = F2(
	function (a, b) {
		return {ctor: 'MySub', _0: a, _1: b};
	});
var _elm_lang$mouse$Mouse$clicks = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'click', tagger));
};
var _elm_lang$mouse$Mouse$moves = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousemove', tagger));
};
var _elm_lang$mouse$Mouse$downs = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousedown', tagger));
};
var _elm_lang$mouse$Mouse$ups = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mouseup', tagger));
};
var _elm_lang$mouse$Mouse$subMap = F2(
	function (func, _p12) {
		var _p13 = _p12;
		return A2(
			_elm_lang$mouse$Mouse$MySub,
			_p13._0,
			function (_p14) {
				return func(
					_p13._1(_p14));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Mouse'] = {pkg: 'elm-lang/mouse', init: _elm_lang$mouse$Mouse$init, onEffects: _elm_lang$mouse$Mouse$onEffects, onSelfMsg: _elm_lang$mouse$Mouse$onSelfMsg, tag: 'sub', subMap: _elm_lang$mouse$Mouse$subMap};

var _debois$elm_mdl$Material_Icon$size48 = A2(_debois$elm_mdl$Material_Options$css, 'font-size', '48px');
var _debois$elm_mdl$Material_Icon$size36 = A2(_debois$elm_mdl$Material_Options$css, 'font-size', '36px');
var _debois$elm_mdl$Material_Icon$size24 = A2(_debois$elm_mdl$Material_Options$css, 'font-size', '24px');
var _debois$elm_mdl$Material_Icon$size18 = A2(_debois$elm_mdl$Material_Options$css, 'font-size', '18px');
var _debois$elm_mdl$Material_Icon$onClick = function (x) {
	return _debois$elm_mdl$Material_Options$set(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{
					onClick: _elm_lang$core$Maybe$Just(
						_elm_lang$html$Html_Events$onClick(x))
				});
		});
};
var _debois$elm_mdl$Material_Icon$defaultConfig = {onClick: _elm_lang$core$Maybe$Nothing};
var _debois$elm_mdl$Material_Icon$view = F2(
	function (name, options) {
		var summary = A2(_debois$elm_mdl$Material_Options$collect, _debois$elm_mdl$Material_Icon$defaultConfig, options);
		return A5(
			_debois$elm_mdl$Material_Options$apply,
			summary,
			_elm_lang$html$Html$i,
			_elm_lang$core$Native_List.fromArray(
				[
					_debois$elm_mdl$Material_Options$cs('material-icons')
				]),
			A2(
				_elm_lang$core$Maybe$withDefault,
				_elm_lang$core$Native_List.fromArray(
					[]),
				A2(
					_elm_lang$core$Maybe$map,
					A2(
						_elm_lang$core$Basics$flip,
						F2(
							function (x, y) {
								return A2(_elm_lang$core$List_ops['::'], x, y);
							}),
						_elm_lang$core$Native_List.fromArray(
							[])),
					summary.config.onClick)),
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html$text(name)
				]));
	});
var _debois$elm_mdl$Material_Icon$i = function (name) {
	return A2(
		_debois$elm_mdl$Material_Icon$view,
		name,
		_elm_lang$core$Native_List.fromArray(
			[]));
};
var _debois$elm_mdl$Material_Icon$Config = function (a) {
	return {onClick: a};
};

var _debois$elm_mdl$Material_Menu_Geometry$Geometry = F5(
	function (a, b, c, d, e) {
		return {button: a, menu: b, container: c, offsetTops: d, offsetHeights: e};
	});
var _debois$elm_mdl$Material_Menu_Geometry$Element = F4(
	function (a, b, c, d) {
		return {offsetTop: a, offsetLeft: b, offsetHeight: c, bounds: d};
	});
var _debois$elm_mdl$Material_Menu_Geometry$element = A5(_elm_lang$core$Json_Decode$object4, _debois$elm_mdl$Material_Menu_Geometry$Element, _debois$elm_dom$DOM$offsetTop, _debois$elm_dom$DOM$offsetLeft, _debois$elm_dom$DOM$offsetHeight, _debois$elm_dom$DOM$boundingClientRect);
var _debois$elm_mdl$Material_Menu_Geometry$decode = A6(
	_elm_lang$core$Json_Decode$object5,
	_debois$elm_mdl$Material_Menu_Geometry$Geometry,
	_debois$elm_dom$DOM$target(_debois$elm_mdl$Material_Menu_Geometry$element),
	_debois$elm_dom$DOM$target(
		_debois$elm_dom$DOM$nextSibling(
			A2(_debois$elm_dom$DOM$childNode, 1, _debois$elm_mdl$Material_Menu_Geometry$element))),
	_debois$elm_dom$DOM$target(
		_debois$elm_dom$DOM$nextSibling(_debois$elm_mdl$Material_Menu_Geometry$element)),
	_debois$elm_dom$DOM$target(
		_debois$elm_dom$DOM$nextSibling(
			A2(
				_debois$elm_dom$DOM$childNode,
				1,
				_debois$elm_dom$DOM$childNodes(_debois$elm_dom$DOM$offsetTop)))),
	_debois$elm_dom$DOM$target(
		_debois$elm_dom$DOM$nextSibling(
			A2(
				_debois$elm_dom$DOM$childNode,
				1,
				_debois$elm_dom$DOM$childNodes(_debois$elm_dom$DOM$offsetHeight)))));

var _debois$elm_mdl$Material_Menu$toPx = function (_p0) {
	return A3(
		_elm_lang$core$Basics$flip,
		F2(
			function (x, y) {
				return A2(_elm_lang$core$Basics_ops['++'], x, y);
			}),
		'px',
		_elm_lang$core$Basics$toString(_p0));
};
var _debois$elm_mdl$Material_Menu$rect = F4(
	function (x, y, w, h) {
		return function (coords) {
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'rect(',
				A2(_elm_lang$core$Basics_ops['++'], coords, ')'));
		}(
			A2(
				_elm_lang$core$String$join,
				' ',
				A2(
					_elm_lang$core$List$map,
					_debois$elm_mdl$Material_Menu$toPx,
					_elm_lang$core$Native_List.fromArray(
						[x, y, w, h]))));
	});
var _debois$elm_mdl$Material_Menu$onKeyDown = function (action) {
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		'keydown',
		{preventDefault: true, stopPropagation: false},
		A2(_elm_lang$core$Json_Decode$map, action, _elm_lang$html$Html_Events$keyCode));
};
var _debois$elm_mdl$Material_Menu$onClick = F2(
	function (decoder, action) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'click',
			A2(_elm_lang$core$Json_Decode$map, action, decoder));
	});
var _debois$elm_mdl$Material_Menu$withGeometry = F2(
	function (model, f) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			_debois$elm_mdl$Material_Options$nop,
			A2(_elm_lang$core$Maybe$map, f, model.geometry));
	});
var _debois$elm_mdl$Material_Menu$icon = function (name) {
	return _debois$elm_mdl$Material_Options$set(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{icon: name});
		});
};
var _debois$elm_mdl$Material_Menu$ripple = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{ripple: true});
	});
var _debois$elm_mdl$Material_Menu$onSelect = function (msg) {
	return _debois$elm_mdl$Material_Options$set(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{
					onSelect: _elm_lang$core$Maybe$Just(msg)
				});
		});
};
var _debois$elm_mdl$Material_Menu$disabled = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{enabled: false});
	});
var _debois$elm_mdl$Material_Menu$divider = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{divider: true});
	});
var _debois$elm_mdl$Material_Menu$defaultItemConfig = {enabled: true, divider: false, onSelect: _elm_lang$core$Maybe$Nothing};
var _debois$elm_mdl$Material_Menu$constant = {transitionDurationSeconds: 0.3, transitionDurationFraction: 0.8, closeTimeout: 150};
var _debois$elm_mdl$Material_Menu$transitionDuration = _debois$elm_mdl$Material_Menu$constant.transitionDurationSeconds * _debois$elm_mdl$Material_Menu$constant.transitionDurationFraction;
var _debois$elm_mdl$Material_Menu$Model = F4(
	function (a, b, c, d) {
		return {ripples: a, animationState: b, geometry: c, index: d};
	});
var _debois$elm_mdl$Material_Menu$Item = F2(
	function (a, b) {
		return {options: a, html: b};
	});
var _debois$elm_mdl$Material_Menu$item = _debois$elm_mdl$Material_Menu$Item;
var _debois$elm_mdl$Material_Menu$ItemConfig = F3(
	function (a, b, c) {
		return {enabled: a, divider: b, onSelect: c};
	});
var _debois$elm_mdl$Material_Menu$Config = F3(
	function (a, b, c) {
		return {alignment: a, ripple: b, icon: c};
	});
var _debois$elm_mdl$Material_Menu$Closing = {ctor: 'Closing'};
var _debois$elm_mdl$Material_Menu$Opened = {ctor: 'Opened'};
var _debois$elm_mdl$Material_Menu$clip = F3(
	function (model, config, geometry) {
		var height = geometry.menu.bounds.height;
		var width = geometry.menu.bounds.width;
		return A2(
			_debois$elm_mdl$Material_Options$css,
			'clip',
			function () {
				if (_elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opened) || _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Closing)) {
					return A4(_debois$elm_mdl$Material_Menu$rect, 0, width, height, 0);
				} else {
					var _p1 = config.alignment;
					switch (_p1.ctor) {
						case 'BottomRight':
							return A4(_debois$elm_mdl$Material_Menu$rect, 0, width, 0, width);
						case 'TopLeft':
							return A4(_debois$elm_mdl$Material_Menu$rect, height, 0, height, 0);
						case 'TopRight':
							return A4(_debois$elm_mdl$Material_Menu$rect, height, width, height, width);
						default:
							return '';
					}
				}
			}());
	});
var _debois$elm_mdl$Material_Menu$Opening = {ctor: 'Opening'};
var _debois$elm_mdl$Material_Menu$isActive = function (model) {
	return _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opened) || _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opening);
};
var _debois$elm_mdl$Material_Menu$Idle = {ctor: 'Idle'};
var _debois$elm_mdl$Material_Menu$defaultModel = {ripples: _elm_lang$core$Dict$empty, animationState: _debois$elm_mdl$Material_Menu$Idle, geometry: _elm_lang$core$Maybe$Nothing, index: _elm_lang$core$Maybe$Nothing};
var _debois$elm_mdl$Material_Menu$Key = F2(
	function (a, b) {
		return {ctor: 'Key', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Menu$Click = function (a) {
	return {ctor: 'Click', _0: a};
};
var _debois$elm_mdl$Material_Menu$subscriptions = function (model) {
	return _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opened) ? _elm_lang$mouse$Mouse$clicks(_debois$elm_mdl$Material_Menu$Click) : _elm_lang$core$Platform_Sub$none;
};
var _debois$elm_mdl$Material_Menu$Ripple = F2(
	function (a, b) {
		return {ctor: 'Ripple', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Menu$Tick = {ctor: 'Tick'};
var _debois$elm_mdl$Material_Menu$Close = {ctor: 'Close'};
var _debois$elm_mdl$Material_Menu$Select = F2(
	function (a, b) {
		return {ctor: 'Select', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Menu$update = F3(
	function (fwd, msg, model) {
		update:
		while (true) {
			var _p2 = msg;
			switch (_p2.ctor) {
				case 'Open':
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								animationState: function () {
									var _p3 = model.animationState;
									if (_p3.ctor === 'Opened') {
										return _debois$elm_mdl$Material_Menu$Opened;
									} else {
										return _debois$elm_mdl$Material_Menu$Opening;
									}
								}(),
								geometry: _elm_lang$core$Maybe$Just(_p2._0)
							}),
						_1: _debois$elm_mdl$Material_Helpers$cmd(
							fwd(_debois$elm_mdl$Material_Menu$Tick))
					};
				case 'Tick':
					return _debois$elm_mdl$Material_Helpers$pure(
						_elm_lang$core$Native_Utils.update(
							model,
							{animationState: _debois$elm_mdl$Material_Menu$Opened}));
				case 'Close':
					return _debois$elm_mdl$Material_Helpers$pure(
						_elm_lang$core$Native_Utils.update(
							model,
							{animationState: _debois$elm_mdl$Material_Menu$Idle, geometry: _elm_lang$core$Maybe$Nothing, index: _elm_lang$core$Maybe$Nothing}));
				case 'Select':
					var cmds = A2(
						_elm_lang$core$List$filterMap,
						_elm_lang$core$Basics$identity,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$core$Maybe$Just(
								A2(
									_debois$elm_mdl$Material_Helpers$delay,
									_debois$elm_mdl$Material_Menu$constant.closeTimeout,
									fwd(_debois$elm_mdl$Material_Menu$Close))),
								A2(_elm_lang$core$Maybe$map, _debois$elm_mdl$Material_Helpers$cmd, _p2._1)
							]));
					var model$ = _elm_lang$core$Native_Utils.update(
						model,
						{animationState: _debois$elm_mdl$Material_Menu$Closing});
					return {
						ctor: '_Tuple2',
						_0: model$,
						_1: _elm_lang$core$Platform_Cmd$batch(cmds)
					};
				case 'Ripple':
					var _p6 = _p2._0;
					var _p4 = A2(
						_debois$elm_mdl$Material_Ripple$update,
						_p2._1,
						A2(
							_elm_lang$core$Maybe$withDefault,
							_debois$elm_mdl$Material_Ripple$model,
							A2(_elm_lang$core$Dict$get, _p6, model.ripples)));
					var model$ = _p4._0;
					var effects = _p4._1;
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								ripples: A3(_elm_lang$core$Dict$insert, _p6, model$, model.ripples)
							}),
						_1: A2(
							_elm_lang$core$Platform_Cmd$map,
							function (_p5) {
								return fwd(
									A2(_debois$elm_mdl$Material_Menu$Ripple, _p6, _p5));
							},
							effects)
					};
				case 'Click':
					if (_debois$elm_mdl$Material_Menu$isActive(model)) {
						var _p7 = model.geometry;
						if (_p7.ctor === 'Just') {
							var inside = F2(
								function (_p9, _p8) {
									var _p10 = _p9;
									var _p15 = _p10.y;
									var _p14 = _p10.x;
									var _p11 = _p8;
									var _p13 = _p11.top;
									var _p12 = _p11.left;
									return (_elm_lang$core$Native_Utils.cmp(
										_p12,
										_elm_lang$core$Basics$toFloat(_p14)) < 1) && ((_elm_lang$core$Native_Utils.cmp(
										_elm_lang$core$Basics$toFloat(_p14),
										_p12 + _p11.width) < 1) && ((_elm_lang$core$Native_Utils.cmp(
										_p13,
										_elm_lang$core$Basics$toFloat(_p15)) < 1) && (_elm_lang$core$Native_Utils.cmp(
										_elm_lang$core$Basics$toFloat(_p15),
										_p13 + _p11.height) < 1)));
								});
							if (A2(inside, _p2._0, _p7._0.menu.bounds)) {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									model,
									_elm_lang$core$Native_List.fromArray(
										[]));
							} else {
								var _v6 = fwd,
									_v7 = _debois$elm_mdl$Material_Menu$Close,
									_v8 = model;
								fwd = _v6;
								msg = _v7;
								model = _v8;
								continue update;
							}
						} else {
							return A2(
								_elm_lang$core$Platform_Cmd_ops['!'],
								model,
								_elm_lang$core$Native_List.fromArray(
									[]));
						}
					} else {
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							model,
							_elm_lang$core$Native_List.fromArray(
								[]));
					}
				default:
					var _p24 = _p2._0;
					var _p16 = _p2._1;
					switch (_p16) {
						case 13:
							if (_debois$elm_mdl$Material_Menu$isActive(model)) {
								var _p17 = model.index;
								if (_p17.ctor === 'Just') {
									var _p19 = _p17._0;
									var cmd = A3(
										_elm_lang$core$Basics$flip,
										_elm_lang$core$Maybe$andThen,
										function (_p18) {
											return function (_) {
												return _.onSelect;
											}(
												function (_) {
													return _.config;
												}(_p18));
										},
										_elm_lang$core$List$head(
											A2(_elm_lang$core$List$drop, _p19, _p24)));
									var _v11 = fwd,
										_v12 = A2(_debois$elm_mdl$Material_Menu$Select, _p19 + 1, cmd),
										_v13 = model;
									fwd = _v11;
									msg = _v12;
									model = _v13;
									continue update;
								} else {
									var _v14 = fwd,
										_v15 = _debois$elm_mdl$Material_Menu$Close,
										_v16 = model;
									fwd = _v14;
									msg = _v15;
									model = _v16;
									continue update;
								}
							} else {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									model,
									_elm_lang$core$Native_List.fromArray(
										[]));
							}
						case 27:
							var _v17 = fwd,
								_v18 = _debois$elm_mdl$Material_Menu$Close,
								_v19 = model;
							fwd = _v17;
							msg = _v18;
							model = _v19;
							continue update;
						case 32:
							if (_debois$elm_mdl$Material_Menu$isActive(model)) {
								var _v20 = fwd,
									_v21 = A2(_debois$elm_mdl$Material_Menu$Key, _p24, 13),
									_v22 = model;
								fwd = _v20;
								msg = _v21;
								model = _v22;
								continue update;
							} else {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									model,
									_elm_lang$core$Native_List.fromArray(
										[]));
							}
						case 40:
							if (_debois$elm_mdl$Material_Menu$isActive(model)) {
								var items = A2(
									_elm_lang$core$List$indexedMap,
									F2(
										function (v0, v1) {
											return {ctor: '_Tuple2', _0: v0, _1: v1};
										}),
									_p24);
								return A3(
									_elm_lang$core$Basics$flip,
									F2(
										function (x, y) {
											return A2(_elm_lang$core$Platform_Cmd_ops['!'], x, y);
										}),
									_elm_lang$core$Native_List.fromArray(
										[]),
									A2(
										_elm_lang$core$Maybe$withDefault,
										model,
										A2(
											_elm_lang$core$Maybe$map,
											function (_p20) {
												return function (index$) {
													return _elm_lang$core$Native_Utils.update(
														model,
														{
															index: _elm_lang$core$Maybe$Just(index$)
														});
												}(
													_elm_lang$core$Basics$fst(_p20));
											},
											_elm_lang$core$List$head(
												A2(
													_elm_lang$core$List$filter,
													function (_p21) {
														return function (_) {
															return _.enabled;
														}(
															function (_) {
																return _.config;
															}(
																_elm_lang$core$Basics$snd(_p21)));
													},
													A2(
														_elm_lang$core$List$drop,
														1 + A2(_elm_lang$core$Maybe$withDefault, -1, model.index),
														A2(_elm_lang$core$Basics_ops['++'], items, items)))))));
							} else {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									model,
									_elm_lang$core$Native_List.fromArray(
										[]));
							}
						case 38:
							if (_debois$elm_mdl$Material_Menu$isActive(model)) {
								var items = A2(
									_elm_lang$core$List$indexedMap,
									F2(
										function (v0, v1) {
											return {ctor: '_Tuple2', _0: v0, _1: v1};
										}),
									_p24);
								return _debois$elm_mdl$Material_Helpers$pure(
									A2(
										_elm_lang$core$Maybe$withDefault,
										model,
										A2(
											_elm_lang$core$Maybe$map,
											function (_p22) {
												return function (index$) {
													return _elm_lang$core$Native_Utils.update(
														model,
														{
															index: _elm_lang$core$Maybe$Just(index$)
														});
												}(
													_elm_lang$core$Basics$fst(_p22));
											},
											_elm_lang$core$List$head(
												A2(
													_elm_lang$core$List$filter,
													function (_p23) {
														return function (_) {
															return _.enabled;
														}(
															function (_) {
																return _.config;
															}(
																_elm_lang$core$Basics$snd(_p23)));
													},
													A2(
														_elm_lang$core$List$drop,
														_elm_lang$core$List$length(_p24) - A2(_elm_lang$core$Maybe$withDefault, 0, model.index),
														_elm_lang$core$List$reverse(
															A2(_elm_lang$core$Basics_ops['++'], items, items))))))));
							} else {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									model,
									_elm_lang$core$Native_List.fromArray(
										[]));
							}
						default:
							return A2(
								_elm_lang$core$Platform_Cmd_ops['!'],
								model,
								_elm_lang$core$Native_List.fromArray(
									[]));
					}
			}
		}
	});
var _debois$elm_mdl$Material_Menu$update$ = F3(
	function (fwd, msg, model) {
		return _elm_lang$core$Maybe$Just(
			A3(_debois$elm_mdl$Material_Menu$update, fwd, msg, model));
	});
var _debois$elm_mdl$Material_Menu$pack = A4(
	_debois$elm_parts$Parts$pack,
	_debois$elm_mdl$Material_Menu$update$,
	function (_) {
		return _.menu;
	},
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.update(
				y,
				{menu: x});
		}),
	_debois$elm_mdl$Material_Menu$defaultModel);
var _debois$elm_mdl$Material_Menu$subs = function (lift) {
	return function (_p25) {
		return _elm_lang$core$Platform_Sub$batch(
			A3(
				_elm_lang$core$Dict$foldl,
				F3(
					function (idx, model, ss) {
						return A2(
							_elm_lang$core$List_ops['::'],
							A2(
								_elm_lang$core$Platform_Sub$map,
								A2(_debois$elm_mdl$Material_Menu$pack, lift, idx),
								_debois$elm_mdl$Material_Menu$subscriptions(model)),
							ss);
					}),
				_elm_lang$core$Native_List.fromArray(
					[]),
				function (_) {
					return _.menu;
				}(_p25)));
	};
};
var _debois$elm_mdl$Material_Menu$Open = function (a) {
	return {ctor: 'Open', _0: a};
};
var _debois$elm_mdl$Material_Menu$TopRight = {ctor: 'TopRight'};
var _debois$elm_mdl$Material_Menu$topRight = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{alignment: _debois$elm_mdl$Material_Menu$TopRight});
	});
var _debois$elm_mdl$Material_Menu$TopLeft = {ctor: 'TopLeft'};
var _debois$elm_mdl$Material_Menu$topLeft = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{alignment: _debois$elm_mdl$Material_Menu$TopLeft});
	});
var _debois$elm_mdl$Material_Menu$delay = F4(
	function (alignment, height, offsetTop, offsetHeight) {
		var t = (_elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopLeft) || _elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopRight)) ? ((((height - offsetTop) - offsetHeight) / height) * _debois$elm_mdl$Material_Menu$transitionDuration) : ((offsetTop / height) * _debois$elm_mdl$Material_Menu$transitionDuration);
		return A2(
			_debois$elm_mdl$Material_Options$css,
			'transition-delay',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(t),
				's'));
	});
var _debois$elm_mdl$Material_Menu$view1 = F8(
	function (lift, config, model, offsetTop, offsetHeight, index, summary, item) {
		var canSelect = summary.config.enabled && (!_elm_lang$core$Native_Utils.eq(summary.config.onSelect, _elm_lang$core$Maybe$Nothing));
		var hasRipple = config.ripple && canSelect;
		var ripple = function (_p26) {
			return lift(
				A2(_debois$elm_mdl$Material_Menu$Ripple, index, _p26));
		};
		return A5(
			_debois$elm_mdl$Material_Options$apply,
			summary,
			_elm_lang$html$Html$li,
			_elm_lang$core$Native_List.fromArray(
				[
					_debois$elm_mdl$Material_Options$cs('mdl-menu__item'),
					A2(
					_debois$elm_mdl$Material_Options$when,
					_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect'),
					config.ripple),
					A2(
					_debois$elm_mdl$Material_Options$when,
					_debois$elm_mdl$Material_Options$cs('mdl-menu__item--full-bleed-divider'),
					summary.config.divider),
					A2(
					_debois$elm_mdl$Material_Options$when,
					A2(_debois$elm_mdl$Material_Options$css, 'background-color', 'rgb(238,238,238)'),
					_elm_lang$core$Native_Utils.eq(
						model.index,
						_elm_lang$core$Maybe$Just(index))),
					function () {
					var _p27 = {
						ctor: '_Tuple2',
						_0: model.geometry,
						_1: _debois$elm_mdl$Material_Menu$isActive(model)
					};
					if (((_p27.ctor === '_Tuple2') && (_p27._0.ctor === 'Just')) && (_p27._1 === true)) {
						return A4(_debois$elm_mdl$Material_Menu$delay, config.alignment, _p27._0._0.menu.bounds.height, offsetTop, offsetHeight);
					} else {
						return _debois$elm_mdl$Material_Options$nop;
					}
				}(),
					A2(_debois$elm_mdl$Material_Options$css, 'display', 'flex'),
					A2(_debois$elm_mdl$Material_Options$css, 'align-items', 'center')
				]),
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(
					_elm_lang$core$List$filterMap,
					_elm_lang$core$Basics$identity,
					_elm_lang$core$Native_List.fromArray(
						[
							canSelect ? _elm_lang$core$Maybe$Just(
							_elm_lang$html$Html_Events$onClick(
								lift(
									A2(_debois$elm_mdl$Material_Menu$Select, index, summary.config.onSelect)))) : _elm_lang$core$Maybe$Nothing,
							_elm_lang$core$Basics$not(summary.config.enabled) ? _elm_lang$core$Maybe$Just(
							A2(_elm_lang$html$Html_Attributes$attribute, 'disabled', 'disabled')) : _elm_lang$core$Maybe$Nothing,
							_elm_lang$core$Maybe$Just(
							A2(
								_elm_lang$html$Html_Attributes$property,
								'tabindex',
								_elm_lang$core$Json_Encode$string('-1')))
						])),
				hasRipple ? _elm_lang$core$Native_List.fromArray(
					[
						A2(_debois$elm_mdl$Material_Ripple$downOn$, ripple, 'mousedown'),
						A2(_debois$elm_mdl$Material_Ripple$downOn$, ripple, 'touchstart'),
						A2(_debois$elm_mdl$Material_Ripple$upOn$, ripple, 'mouseup'),
						A2(_debois$elm_mdl$Material_Ripple$upOn$, ripple, 'mouseleave'),
						A2(_debois$elm_mdl$Material_Ripple$upOn$, ripple, 'touchend'),
						A2(_debois$elm_mdl$Material_Ripple$upOn$, ripple, 'blur')
					]) : _elm_lang$core$Native_List.fromArray(
					[])),
			hasRipple ? A2(
				F2(
					function (x, y) {
						return A2(_elm_lang$core$Basics_ops['++'], x, y);
					}),
				item.html,
				_elm_lang$core$Native_List.fromArray(
					[
						A2(
						_elm_lang$html$Html_App$map,
						ripple,
						A2(
							_debois$elm_mdl$Material_Ripple$view$,
							_elm_lang$core$Native_List.fromArray(
								[
									_elm_lang$html$Html_Attributes$class('mdl-menu__item-ripple-container')
								]),
							A2(
								_elm_lang$core$Maybe$withDefault,
								_debois$elm_mdl$Material_Ripple$model,
								A2(_elm_lang$core$Dict$get, index, model.ripples))))
					])) : item.html);
	});
var _debois$elm_mdl$Material_Menu$BottomRight = {ctor: 'BottomRight'};
var _debois$elm_mdl$Material_Menu$bottomRight = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{alignment: _debois$elm_mdl$Material_Menu$BottomRight});
	});
var _debois$elm_mdl$Material_Menu$BottomLeft = {ctor: 'BottomLeft'};
var _debois$elm_mdl$Material_Menu$defaultConfig = {alignment: _debois$elm_mdl$Material_Menu$BottomLeft, ripple: false, icon: 'more_vert'};
var _debois$elm_mdl$Material_Menu$bottomLeft = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{alignment: _debois$elm_mdl$Material_Menu$BottomLeft});
	});
var _debois$elm_mdl$Material_Menu$containerGeometry = F2(
	function (alignment, geometry) {
		return _debois$elm_mdl$Material_Options$many(
			_elm_lang$core$Native_List.fromArray(
				[
					A2(
					_debois$elm_mdl$Material_Options$css,
					'width',
					_debois$elm_mdl$Material_Menu$toPx(geometry.menu.bounds.width)),
					A2(
					_debois$elm_mdl$Material_Options$css,
					'height',
					_debois$elm_mdl$Material_Menu$toPx(geometry.menu.bounds.height)),
					(_elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$BottomRight) || _elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$BottomLeft)) ? A2(
					_debois$elm_mdl$Material_Options$css,
					'top',
					_debois$elm_mdl$Material_Menu$toPx(geometry.button.offsetTop + geometry.button.offsetHeight)) : _debois$elm_mdl$Material_Options$nop,
					function () {
					if (_elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$BottomRight) || _elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopRight)) {
						var right = function (e) {
							return e.bounds.left + e.bounds.width;
						};
						return A2(
							_debois$elm_mdl$Material_Options$css,
							'right',
							_debois$elm_mdl$Material_Menu$toPx(
								right(geometry.container) - right(geometry.menu)));
					} else {
						return _debois$elm_mdl$Material_Options$nop;
					}
				}(),
					function () {
					if (_elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopLeft) || _elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopRight)) {
						var bottom = geometry.container.bounds.top + geometry.container.bounds.height;
						return A2(
							_debois$elm_mdl$Material_Options$css,
							'bottom',
							_debois$elm_mdl$Material_Menu$toPx(bottom - geometry.button.bounds.top));
					} else {
						return _debois$elm_mdl$Material_Options$nop;
					}
				}(),
					(_elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopLeft) || _elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$BottomLeft)) ? A2(
					_debois$elm_mdl$Material_Options$css,
					'left',
					_debois$elm_mdl$Material_Menu$toPx(geometry.menu.offsetLeft)) : _debois$elm_mdl$Material_Options$nop
				]));
	});
var _debois$elm_mdl$Material_Menu$view = F4(
	function (lift, model, properties, items) {
		var itemSummaries = A2(
			_elm_lang$core$List$map,
			function (_p28) {
				return A2(
					_debois$elm_mdl$Material_Options$collect,
					_debois$elm_mdl$Material_Menu$defaultItemConfig,
					function (_) {
						return _.options;
					}(_p28));
			},
			items);
		var numItems = _elm_lang$core$List$length(items);
		var summary = A2(_debois$elm_mdl$Material_Options$collect, _debois$elm_mdl$Material_Menu$defaultConfig, properties);
		var config = summary.config;
		var alignment = function () {
			var _p29 = config.alignment;
			switch (_p29.ctor) {
				case 'BottomLeft':
					return _debois$elm_mdl$Material_Options$cs('mdl-menu--bottom-left');
				case 'BottomRight':
					return _debois$elm_mdl$Material_Options$cs('mdl-menu--bottom-right');
				case 'TopLeft':
					return _debois$elm_mdl$Material_Options$cs('mdl-menu--top-left');
				default:
					return _debois$elm_mdl$Material_Options$cs('mdl-menu--top-right');
			}
		}();
		return A5(
			_debois$elm_mdl$Material_Options$apply,
			summary,
			_elm_lang$html$Html$div,
			A2(
				_elm_lang$core$List_ops['::'],
				A2(_debois$elm_mdl$Material_Options$css, 'position', 'relative'),
				properties),
			_elm_lang$core$Native_List.fromArray(
				[]),
			_elm_lang$core$Native_List.fromArray(
				[
					A2(
					_elm_lang$html$Html_App$map,
					lift,
					A3(
						_debois$elm_mdl$Material_Options$styled,
						_elm_lang$html$Html$button,
						_elm_lang$core$Native_List.fromArray(
							[
								_debois$elm_mdl$Material_Options$cs('mdl-button'),
								_debois$elm_mdl$Material_Options$cs('mdl-js-button'),
								_debois$elm_mdl$Material_Options$cs('mdl-button--icon'),
								A2(
								_debois$elm_mdl$Material_Options$when,
								_debois$elm_mdl$Material_Options_Internal$attribute(
									_debois$elm_mdl$Material_Menu$onKeyDown(
										_debois$elm_mdl$Material_Menu$Key(itemSummaries))),
								_debois$elm_mdl$Material_Menu$isActive(model)),
								A2(
								_debois$elm_mdl$Material_Options$when,
								_debois$elm_mdl$Material_Options_Internal$attribute(
									A2(_debois$elm_mdl$Material_Menu$onClick, _debois$elm_mdl$Material_Menu_Geometry$decode, _debois$elm_mdl$Material_Menu$Open)),
								!_elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opened)),
								A2(
								_debois$elm_mdl$Material_Options$when,
								_debois$elm_mdl$Material_Options_Internal$attribute(
									_elm_lang$html$Html_Events$onClick(_debois$elm_mdl$Material_Menu$Close)),
								_debois$elm_mdl$Material_Menu$isActive(model))
							]),
						_elm_lang$core$Native_List.fromArray(
							[
								A2(
								_debois$elm_mdl$Material_Icon$view,
								config.icon,
								_elm_lang$core$Native_List.fromArray(
									[
										_debois$elm_mdl$Material_Options$cs('material-icons'),
										A2(_debois$elm_mdl$Material_Options$css, 'pointer-events', 'none')
									]))
							]))),
					A3(
					_debois$elm_mdl$Material_Options$styled,
					_elm_lang$html$Html$div,
					_elm_lang$core$Native_List.fromArray(
						[
							_debois$elm_mdl$Material_Options$cs('mdl-menu__container'),
							_debois$elm_mdl$Material_Options$cs('is-upgraded'),
							A2(
							_debois$elm_mdl$Material_Options$when,
							_debois$elm_mdl$Material_Options$cs('is-visible'),
							_elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opened) || _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Closing)),
							A2(
							_debois$elm_mdl$Material_Menu$withGeometry,
							model,
							_debois$elm_mdl$Material_Menu$containerGeometry(config.alignment))
						]),
					_elm_lang$core$Native_List.fromArray(
						[
							A3(
							_debois$elm_mdl$Material_Options$styled,
							_elm_lang$html$Html$div,
							_elm_lang$core$Native_List.fromArray(
								[
									_debois$elm_mdl$Material_Options$cs('mdl-menu__outline'),
									alignment,
									A2(
									_debois$elm_mdl$Material_Menu$withGeometry,
									model,
									function (geometry) {
										return _debois$elm_mdl$Material_Options$many(
											_elm_lang$core$Native_List.fromArray(
												[
													A2(
													_debois$elm_mdl$Material_Options$css,
													'width',
													_debois$elm_mdl$Material_Menu$toPx(geometry.menu.bounds.width)),
													A2(
													_debois$elm_mdl$Material_Options$css,
													'height',
													_debois$elm_mdl$Material_Menu$toPx(geometry.menu.bounds.height))
												]));
									})
								]),
							_elm_lang$core$Native_List.fromArray(
								[])),
							A3(
							_debois$elm_mdl$Material_Options$styled,
							_elm_lang$html$Html$ul,
							_elm_lang$core$Native_List.fromArray(
								[
									_debois$elm_mdl$Material_Options$cs('mdl-menu'),
									_debois$elm_mdl$Material_Options$cs('mdl-js-menu'),
									A2(
									_debois$elm_mdl$Material_Options$when,
									_debois$elm_mdl$Material_Options$cs('is-animating'),
									_elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opening) || _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Closing)),
									A2(
									_debois$elm_mdl$Material_Menu$withGeometry,
									model,
									A2(_debois$elm_mdl$Material_Menu$clip, model, config)),
									alignment
								]),
							function () {
								var _p30 = model.geometry;
								if (_p30.ctor === 'Just') {
									var _p31 = _p30._0;
									return A6(
										_elm_lang$core$List$map5,
										A3(_debois$elm_mdl$Material_Menu$view1, lift, config, model),
										_p31.offsetTops,
										_p31.offsetHeights,
										_elm_lang$core$Native_List.range(0, numItems - 1),
										itemSummaries,
										items);
								} else {
									return A4(
										_elm_lang$core$List$map3,
										A5(_debois$elm_mdl$Material_Menu$view1, lift, config, model, 0, 0),
										_elm_lang$core$Native_List.range(0, numItems - 1),
										itemSummaries,
										items);
								}
							}())
						]))
				]));
	});
var _debois$elm_mdl$Material_Menu$render = A5(
	_debois$elm_parts$Parts$create,
	_debois$elm_mdl$Material_Menu$view,
	_debois$elm_mdl$Material_Menu$update$,
	function (_) {
		return _.menu;
	},
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.update(
				y,
				{menu: x});
		}),
	_debois$elm_mdl$Material_Menu$defaultModel);

var _debois$elm_mdl$Material_Snackbar$enqueue = F2(
	function (contents, model) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				queue: A2(
					_elm_lang$core$List$append,
					model.queue,
					_elm_lang$core$Native_List.fromArray(
						[contents]))
			});
	});
var _debois$elm_mdl$Material_Snackbar$snackbar = F3(
	function (payload, message, label) {
		return {
			message: message,
			action: _elm_lang$core$Maybe$Just(label),
			payload: payload,
			timeout: 2750,
			fade: 250
		};
	});
var _debois$elm_mdl$Material_Snackbar$toast = F2(
	function (payload, message) {
		return {message: message, action: _elm_lang$core$Maybe$Nothing, payload: payload, timeout: 2750, fade: 250};
	});
var _debois$elm_mdl$Material_Snackbar$Contents = F5(
	function (a, b, c, d, e) {
		return {message: a, action: b, payload: c, timeout: d, fade: e};
	});
var _debois$elm_mdl$Material_Snackbar$Model = F3(
	function (a, b, c) {
		return {queue: a, state: b, seq: c};
	});
var _debois$elm_mdl$Material_Snackbar$Fading = function (a) {
	return {ctor: 'Fading', _0: a};
};
var _debois$elm_mdl$Material_Snackbar$Active = function (a) {
	return {ctor: 'Active', _0: a};
};
var _debois$elm_mdl$Material_Snackbar$Inert = {ctor: 'Inert'};
var _debois$elm_mdl$Material_Snackbar$model = {
	queue: _elm_lang$core$Native_List.fromArray(
		[]),
	state: _debois$elm_mdl$Material_Snackbar$Inert,
	seq: -1
};
var _debois$elm_mdl$Material_Snackbar$Clicked = {ctor: 'Clicked'};
var _debois$elm_mdl$Material_Snackbar$Timeout = {ctor: 'Timeout'};
var _debois$elm_mdl$Material_Snackbar$Move = F2(
	function (a, b) {
		return {ctor: 'Move', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Snackbar$next = function (model) {
	return _elm_lang$core$Platform_Cmd$map(
		_debois$elm_mdl$Material_Snackbar$Move(model.seq));
};
var _debois$elm_mdl$Material_Snackbar$view = function (model) {
	var isActive = function () {
		var _p0 = model.state;
		switch (_p0.ctor) {
			case 'Inert':
				return false;
			case 'Active':
				return true;
			default:
				return false;
		}
	}();
	var contents = function () {
		var _p1 = model.state;
		switch (_p1.ctor) {
			case 'Inert':
				return _elm_lang$core$Maybe$Nothing;
			case 'Active':
				return _elm_lang$core$Maybe$Just(_p1._0);
			default:
				return _elm_lang$core$Maybe$Just(_p1._0);
		}
	}();
	var action = A2(
		_elm_lang$core$Maybe$andThen,
		contents,
		function (_) {
			return _.action;
		});
	return A2(
		_elm_lang$html$Html$div,
		_elm_lang$core$Native_List.fromArray(
			[
				_elm_lang$html$Html_Attributes$classList(
				_elm_lang$core$Native_List.fromArray(
					[
						{ctor: '_Tuple2', _0: 'mdl-js-snackbar', _1: true},
						{ctor: '_Tuple2', _0: 'mdl-snackbar', _1: true},
						{ctor: '_Tuple2', _0: 'mdl-snackbar--active', _1: isActive}
					])),
				A2(
				_debois$elm_mdl$Material_Helpers$aria,
				'hidden',
				_elm_lang$core$Basics$not(isActive))
			]),
		_elm_lang$core$Native_List.fromArray(
			[
				A2(
				_elm_lang$html$Html$div,
				_elm_lang$core$Native_List.fromArray(
					[
						_elm_lang$html$Html_Attributes$class('mdl-snackbar__text')
					]),
				A2(
					_elm_lang$core$Maybe$withDefault,
					_elm_lang$core$Native_List.fromArray(
						[]),
					A2(
						_elm_lang$core$Maybe$map,
						function (c) {
							return _elm_lang$core$Native_List.fromArray(
								[
									_elm_lang$html$Html$text(c.message)
								]);
						},
						contents))),
				A2(
				_elm_lang$html$Html$button,
				A2(
					_elm_lang$core$List_ops['::'],
					_elm_lang$html$Html_Attributes$class('mdl-snackbar__action'),
					A2(
						_elm_lang$core$List_ops['::'],
						_elm_lang$html$Html_Attributes$type$('button'),
						A2(
							_elm_lang$core$List_ops['::'],
							A2(
								_debois$elm_mdl$Material_Helpers$aria,
								'hidden',
								A2(
									_elm_lang$core$Maybe$withDefault,
									true,
									A2(
										_elm_lang$core$Maybe$map,
										_elm_lang$core$Basics$always(
											_elm_lang$core$Basics$not(isActive)),
										action))),
							A2(
								_elm_lang$core$Maybe$withDefault,
								_elm_lang$core$Native_List.fromArray(
									[]),
								A2(
									_elm_lang$core$Maybe$map,
									_elm_lang$core$Basics$always(
										_elm_lang$core$Native_List.fromArray(
											[
												_elm_lang$html$Html_Events$onClick(
												A2(_debois$elm_mdl$Material_Snackbar$Move, model.seq, _debois$elm_mdl$Material_Snackbar$Clicked))
											])),
									action))))),
				A2(
					_elm_lang$core$Maybe$withDefault,
					_elm_lang$core$Native_List.fromArray(
						[]),
					A2(
						_elm_lang$core$Maybe$map,
						function (action) {
							return _elm_lang$core$Native_List.fromArray(
								[
									_elm_lang$html$Html$text(action)
								]);
						},
						action)))
			]));
};
var _debois$elm_mdl$Material_Snackbar$Click = function (a) {
	return {ctor: 'Click', _0: a};
};
var _debois$elm_mdl$Material_Snackbar$End = function (a) {
	return {ctor: 'End', _0: a};
};
var _debois$elm_mdl$Material_Snackbar$Begin = function (a) {
	return {ctor: 'Begin', _0: a};
};
var _debois$elm_mdl$Material_Snackbar$tryDequeue = function (model) {
	var _p2 = {ctor: '_Tuple2', _0: model.state, _1: model.queue};
	if (((_p2.ctor === '_Tuple2') && (_p2._0.ctor === 'Inert')) && (_p2._1.ctor === '::')) {
		var _p3 = _p2._1._0;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Native_Utils.update(
				model,
				{
					state: _debois$elm_mdl$Material_Snackbar$Active(_p3),
					queue: _p2._1._1,
					seq: model.seq + 1
				}),
			_1: _elm_lang$core$Platform_Cmd$batch(
				_elm_lang$core$Native_List.fromArray(
					[
						A2(
						_elm_lang$core$Platform_Cmd$map,
						_debois$elm_mdl$Material_Snackbar$Move(model.seq + 1),
						A2(_debois$elm_mdl$Material_Helpers$delay, _p3.timeout, _debois$elm_mdl$Material_Snackbar$Timeout)),
						_debois$elm_mdl$Material_Helpers$cmd(
						_debois$elm_mdl$Material_Snackbar$Begin(_p3.payload))
					]))
		};
	} else {
		return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
	}
};
var _debois$elm_mdl$Material_Snackbar$move = F2(
	function (transition, model) {
		var _p4 = {ctor: '_Tuple2', _0: model.state, _1: transition};
		_v3_4:
		do {
			if (_p4.ctor === '_Tuple2') {
				if (_p4._1.ctor === 'Clicked') {
					if (_p4._0.ctor === 'Active') {
						var _p5 = _p4._0._0;
						return {
							ctor: '_Tuple2',
							_0: _elm_lang$core$Native_Utils.update(
								model,
								{
									state: _debois$elm_mdl$Material_Snackbar$Fading(_p5)
								}),
							_1: _elm_lang$core$Platform_Cmd$batch(
								_elm_lang$core$Native_List.fromArray(
									[
										A2(
										_debois$elm_mdl$Material_Snackbar$next,
										model,
										A2(_debois$elm_mdl$Material_Helpers$delay, _p5.fade, _debois$elm_mdl$Material_Snackbar$Timeout)),
										_debois$elm_mdl$Material_Helpers$cmd(
										_debois$elm_mdl$Material_Snackbar$Click(_p5.payload))
									]))
						};
					} else {
						break _v3_4;
					}
				} else {
					switch (_p4._0.ctor) {
						case 'Inert':
							return _debois$elm_mdl$Material_Snackbar$tryDequeue(model);
						case 'Active':
							var _p6 = _p4._0._0;
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Native_Utils.update(
									model,
									{
										state: _debois$elm_mdl$Material_Snackbar$Fading(_p6)
									}),
								_1: _elm_lang$core$Platform_Cmd$batch(
									_elm_lang$core$Native_List.fromArray(
										[
											A2(
											_debois$elm_mdl$Material_Snackbar$next,
											model,
											A2(_debois$elm_mdl$Material_Helpers$delay, _p6.fade, _debois$elm_mdl$Material_Snackbar$Timeout))
										]))
							};
						default:
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Native_Utils.update(
									model,
									{state: _debois$elm_mdl$Material_Snackbar$Inert}),
								_1: _elm_lang$core$Platform_Cmd$batch(
									_elm_lang$core$Native_List.fromArray(
										[
											A2(
											_debois$elm_mdl$Material_Snackbar$next,
											model,
											_debois$elm_mdl$Material_Helpers$cmd(_debois$elm_mdl$Material_Snackbar$Timeout)),
											_debois$elm_mdl$Material_Helpers$cmd(
											_debois$elm_mdl$Material_Snackbar$End(_p4._0._0.payload))
										]))
							};
					}
				}
			} else {
				break _v3_4;
			}
		} while(false);
		return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
	});
var _debois$elm_mdl$Material_Snackbar$update = F2(
	function (action, model) {
		var _p7 = action;
		if (_p7.ctor === 'Move') {
			return _elm_lang$core$Native_Utils.eq(_p7._0, model.seq) ? A2(_debois$elm_mdl$Material_Snackbar$move, _p7._1, model) : {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
		} else {
			return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
		}
	});
var _debois$elm_mdl$Material_Snackbar$add = F2(
	function (contents, model) {
		return _debois$elm_mdl$Material_Snackbar$tryDequeue(
			A2(_debois$elm_mdl$Material_Snackbar$enqueue, contents, model));
	});

var _elm_lang$html$Html_Keyed$node = _elm_lang$virtual_dom$VirtualDom$keyedNode;
var _elm_lang$html$Html_Keyed$ol = _elm_lang$html$Html_Keyed$node('ol');
var _elm_lang$html$Html_Keyed$ul = _elm_lang$html$Html_Keyed$node('ul');

var _elm_lang$window$Native_Window = function()
{

var size = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)	{
	callback(_elm_lang$core$Native_Scheduler.succeed({
		width: window.innerWidth,
		height: window.innerHeight
	}));
});

return {
	size: size
};

}();
var _elm_lang$window$Window_ops = _elm_lang$window$Window_ops || {};
_elm_lang$window$Window_ops['&>'] = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Task$andThen,
			t1,
			function (_p0) {
				return t2;
			});
	});
var _elm_lang$window$Window$onSelfMsg = F3(
	function (router, dimensions, state) {
		var _p1 = state;
		if (_p1.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (_p2) {
				var _p3 = _p2;
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					_p3._0(dimensions));
			};
			return A2(
				_elm_lang$window$Window_ops['&>'],
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p1._0.subs)),
				_elm_lang$core$Task$succeed(state));
		}
	});
var _elm_lang$window$Window$init = _elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing);
var _elm_lang$window$Window$size = _elm_lang$window$Native_Window.size;
var _elm_lang$window$Window$width = A2(
	_elm_lang$core$Task$map,
	function (_) {
		return _.width;
	},
	_elm_lang$window$Window$size);
var _elm_lang$window$Window$height = A2(
	_elm_lang$core$Task$map,
	function (_) {
		return _.height;
	},
	_elm_lang$window$Window$size);
var _elm_lang$window$Window$onEffects = F3(
	function (router, newSubs, oldState) {
		var _p4 = {ctor: '_Tuple2', _0: oldState, _1: newSubs};
		if (_p4._0.ctor === 'Nothing') {
			if (_p4._1.ctor === '[]') {
				return _elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing);
			} else {
				return A2(
					_elm_lang$core$Task$andThen,
					_elm_lang$core$Process$spawn(
						A3(
							_elm_lang$dom$Dom_LowLevel$onWindow,
							'resize',
							_elm_lang$core$Json_Decode$succeed(
								{ctor: '_Tuple0'}),
							function (_p5) {
								return A2(
									_elm_lang$core$Task$andThen,
									_elm_lang$window$Window$size,
									_elm_lang$core$Platform$sendToSelf(router));
							})),
					function (pid) {
						return _elm_lang$core$Task$succeed(
							_elm_lang$core$Maybe$Just(
								{subs: newSubs, pid: pid}));
					});
			}
		} else {
			if (_p4._1.ctor === '[]') {
				return A2(
					_elm_lang$window$Window_ops['&>'],
					_elm_lang$core$Process$kill(_p4._0._0.pid),
					_elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing));
			} else {
				return _elm_lang$core$Task$succeed(
					_elm_lang$core$Maybe$Just(
						{subs: newSubs, pid: _p4._0._0.pid}));
			}
		}
	});
var _elm_lang$window$Window$subscription = _elm_lang$core$Native_Platform.leaf('Window');
var _elm_lang$window$Window$Size = F2(
	function (a, b) {
		return {width: a, height: b};
	});
var _elm_lang$window$Window$MySub = function (a) {
	return {ctor: 'MySub', _0: a};
};
var _elm_lang$window$Window$resizes = function (tagger) {
	return _elm_lang$window$Window$subscription(
		_elm_lang$window$Window$MySub(tagger));
};
var _elm_lang$window$Window$subMap = F2(
	function (func, _p6) {
		var _p7 = _p6;
		return _elm_lang$window$Window$MySub(
			function (_p8) {
				return func(
					_p7._0(_p8));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Window'] = {pkg: 'elm-lang/window', init: _elm_lang$window$Window$init, onEffects: _elm_lang$window$Window$onEffects, onSelfMsg: _elm_lang$window$Window$onSelfMsg, tag: 'sub', subMap: _elm_lang$window$Window$subMap};

var _debois$elm_mdl$Material_Layout$drawerView = F3(
	function (lift, isVisible, elems) {
		return A2(
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html_Attributes$classList(
					_elm_lang$core$Native_List.fromArray(
						[
							{ctor: '_Tuple2', _0: 'mdl-layout__drawer', _1: true},
							{ctor: '_Tuple2', _0: 'is-visible', _1: isVisible}
						])),
					A2(
					_elm_lang$html$Html_Attributes$attribute,
					'aria-hidden',
					isVisible ? 'false' : 'true')
				]),
			elems);
	});
var _debois$elm_mdl$Material_Layout$onKeypressFilterSpaceAndEnter = A2(_elm_lang$html$Html_Attributes$attribute, 'onkeypress', '\n  (function (evt) {\n     if (evt && evt.type === \"keydown\" && (evt.keyCode === 32 || evt.keyCode === 13)) {\n       evt.preventDefault();\n     }\n   })(window.event);\n  ');
var _debois$elm_mdl$Material_Layout$toList = function (x) {
	var _p0 = x;
	if (_p0.ctor === 'Nothing') {
		return _elm_lang$core$Native_List.fromArray(
			[]);
	} else {
		return _elm_lang$core$Native_List.fromArray(
			[_p0._0]);
	}
};
var _debois$elm_mdl$Material_Layout$isWaterfall = function (mode) {
	var _p1 = mode;
	if (_p1.ctor === 'Waterfall') {
		return true;
	} else {
		return false;
	}
};
var _debois$elm_mdl$Material_Layout$row = function (styles) {
	return _debois$elm_mdl$Material_Options$div(
		A2(
			_elm_lang$core$List_ops['::'],
			_debois$elm_mdl$Material_Options$cs('mdl-layout__header-row'),
			styles));
};
var _debois$elm_mdl$Material_Layout$link = F2(
	function (styles, contents) {
		return A3(
			_debois$elm_mdl$Material_Options$styled,
			_elm_lang$html$Html$a,
			A2(
				_elm_lang$core$List_ops['::'],
				_debois$elm_mdl$Material_Options$cs('mdl-navigation__link'),
				A2(
					_elm_lang$core$List_ops['::'],
					_debois$elm_mdl$Material_Options_Internal$attribute(
						A2(_elm_lang$html$Html_Attributes$attribute, 'tabindex', '1')),
					styles)),
			contents);
	});
var _debois$elm_mdl$Material_Layout$href = function (_p2) {
	return _debois$elm_mdl$Material_Options_Internal$attribute(
		_elm_lang$html$Html_Attributes$href(_p2));
};
var _debois$elm_mdl$Material_Layout$onClick = function (_p3) {
	return _debois$elm_mdl$Material_Options_Internal$attribute(
		_elm_lang$html$Html_Events$onClick(_p3));
};
var _debois$elm_mdl$Material_Layout$navigation = F2(
	function (styles, contents) {
		return A2(
			_elm_lang$html$Html$nav,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html_Attributes$class('mdl-navigation')
				]),
			contents);
	});
var _debois$elm_mdl$Material_Layout$title = function (styles) {
	return _debois$elm_mdl$Material_Options$span(
		A2(
			_elm_lang$core$List_ops['::'],
			_debois$elm_mdl$Material_Options$cs('mdl-layout__title'),
			styles));
};
var _debois$elm_mdl$Material_Layout$spacer = A2(
	_elm_lang$html$Html$div,
	_elm_lang$core$Native_List.fromArray(
		[
			_elm_lang$html$Html_Attributes$class('mdl-layout-spacer')
		]),
	_elm_lang$core$Native_List.fromArray(
		[]));
var _debois$elm_mdl$Material_Layout$onSelectTab = function (f) {
	return _debois$elm_mdl$Material_Options$set(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{
					onSelectTab: _elm_lang$core$Maybe$Just(
						function (_p4) {
							return _elm_lang$html$Html_Events$onClick(
								f(_p4));
						})
				});
		});
};
var _debois$elm_mdl$Material_Layout$moreTabs = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{moreTabs: true});
	});
var _debois$elm_mdl$Material_Layout$selectedTab = function (k) {
	return _debois$elm_mdl$Material_Options$set(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{selectedTab: k});
		});
};
var _debois$elm_mdl$Material_Layout$transparentHeader = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{transparentHeader: true});
	});
var _debois$elm_mdl$Material_Layout$rippleTabs = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{rippleTabs: true});
	});
var _debois$elm_mdl$Material_Layout$fixedTabs = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{fixedTabs: true});
	});
var _debois$elm_mdl$Material_Layout$fixedDrawer = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{fixedDrawer: true});
	});
var _debois$elm_mdl$Material_Layout$fixedHeader = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{fixedHeader: true});
	});
var _debois$elm_mdl$Material_Layout$setTabsWidth$ = F2(
	function (width, model) {
		var x = model.tabScrollState;
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				tabScrollState: _elm_lang$core$Native_Utils.update(
					x,
					{
						width: _elm_lang$core$Maybe$Just(width)
					})
			});
	});
var _debois$elm_mdl$Material_Layout$setTabsWidth = F2(
	function (w, container) {
		return _elm_lang$core$Native_Utils.update(
			container,
			{
				layout: A2(_debois$elm_mdl$Material_Layout$setTabsWidth$, w, container.layout)
			});
	});
var _debois$elm_mdl$Material_Layout$defaultTabScrollState = {canScrollRight: true, canScrollLeft: false, width: _elm_lang$core$Maybe$Nothing};
var _debois$elm_mdl$Material_Layout$defaultModel = {ripples: _elm_lang$core$Dict$empty, isSmallScreen: false, isCompact: false, isAnimating: false, isScrolled: false, isDrawerOpen: false, tabScrollState: _debois$elm_mdl$Material_Layout$defaultTabScrollState};
var _debois$elm_mdl$Material_Layout$TabScrollState = F3(
	function (a, b, c) {
		return {canScrollLeft: a, canScrollRight: b, width: c};
	});
var _debois$elm_mdl$Material_Layout$Model = F7(
	function (a, b, c, d, e, f, g) {
		return {ripples: a, isSmallScreen: b, isCompact: c, isAnimating: d, isScrolled: e, isDrawerOpen: f, tabScrollState: g};
	});
var _debois$elm_mdl$Material_Layout$Config = F9(
	function (a, b, c, d, e, f, g, h, i) {
		return {fixedHeader: a, fixedDrawer: b, fixedTabs: c, rippleTabs: d, mode: e, selectedTab: f, onSelectTab: g, transparentHeader: h, moreTabs: i};
	});
var _debois$elm_mdl$Material_Layout$Contents = F4(
	function (a, b, c, d) {
		return {header: a, drawer: b, tabs: c, main: d};
	});
var _debois$elm_mdl$Material_Layout$Ripple = F2(
	function (a, b) {
		return {ctor: 'Ripple', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Layout$NOP = {ctor: 'NOP'};
var _debois$elm_mdl$Material_Layout$TransitionEnd = {ctor: 'TransitionEnd'};
var _debois$elm_mdl$Material_Layout$TransitionHeader = function (a) {
	return {ctor: 'TransitionHeader', _0: a};
};
var _debois$elm_mdl$Material_Layout$update$ = F3(
	function (f, action, model) {
		update$:
		while (true) {
			var _p5 = action;
			switch (_p5.ctor) {
				case 'NOP':
					return _elm_lang$core$Maybe$Nothing;
				case 'Resize':
					var _p6 = _p5._0;
					var tabScrollState = A2(
						_elm_lang$core$Maybe$withDefault,
						model.tabScrollState,
						A2(
							_elm_lang$core$Maybe$map,
							function (tabsWidth) {
								var tabScrollState = model.tabScrollState;
								return _elm_lang$core$Native_Utils.update(
									tabScrollState,
									{
										canScrollRight: _elm_lang$core$Native_Utils.cmp(tabsWidth + (2 * 56), _p6) > 0
									});
							},
							model.tabScrollState.width));
					var isSmall = _elm_lang$core$Native_Utils.cmp(1024, _p6) > 0;
					return (_elm_lang$core$Native_Utils.eq(isSmall, model.isSmallScreen) && _elm_lang$core$Native_Utils.eq(tabScrollState.canScrollRight, model.tabScrollState.canScrollRight)) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
						_debois$elm_mdl$Material_Helpers$pure(
							_elm_lang$core$Native_Utils.update(
								model,
								{
									isSmallScreen: isSmall,
									isDrawerOpen: _elm_lang$core$Basics$not(isSmall) && model.isDrawerOpen,
									tabScrollState: tabScrollState
								})));
				case 'ToggleDrawer':
					return _elm_lang$core$Maybe$Just(
						_debois$elm_mdl$Material_Helpers$pure(
							_elm_lang$core$Native_Utils.update(
								model,
								{
									isDrawerOpen: _elm_lang$core$Basics$not(model.isDrawerOpen)
								})));
				case 'Ripple':
					var _p8 = _p5._0;
					return _elm_lang$core$Maybe$Just(
						A2(
							_debois$elm_mdl$Material_Helpers$map2nd,
							_elm_lang$core$Platform_Cmd$map(
								function (_p7) {
									return f(
										A2(_debois$elm_mdl$Material_Layout$Ripple, _p8, _p7));
								}),
							A2(
								_debois$elm_mdl$Material_Helpers$map1st,
								function (ripple$) {
									return _elm_lang$core$Native_Utils.update(
										model,
										{
											ripples: A3(_elm_lang$core$Dict$insert, _p8, ripple$, model.ripples)
										});
								},
								A2(
									_debois$elm_mdl$Material_Ripple$update,
									_p5._1,
									A2(
										_elm_lang$core$Maybe$withDefault,
										_debois$elm_mdl$Material_Ripple$model,
										A2(_elm_lang$core$Dict$get, _p8, model.ripples))))));
				case 'ScrollTab':
					var _p9 = _p5._0;
					return (!_elm_lang$core$Native_Utils.eq(model.tabScrollState, _p9)) ? _elm_lang$core$Maybe$Just(
						_debois$elm_mdl$Material_Helpers$pure(
							_elm_lang$core$Native_Utils.update(
								model,
								{tabScrollState: _p9}))) : _elm_lang$core$Maybe$Nothing;
				case 'ScrollPane':
					var isScrolled = _elm_lang$core$Native_Utils.cmp(0.0, _p5._1) < 0;
					if (!_elm_lang$core$Native_Utils.eq(isScrolled, model.isScrolled)) {
						var _v3 = f,
							_v4 = _debois$elm_mdl$Material_Layout$TransitionHeader(
							{toCompact: isScrolled, fixedHeader: _p5._0}),
							_v5 = _elm_lang$core$Native_Utils.update(
							model,
							{isScrolled: isScrolled});
						f = _v3;
						action = _v4;
						model = _v5;
						continue update$;
					} else {
						return _elm_lang$core$Maybe$Nothing;
					}
				case 'TransitionHeader':
					return _elm_lang$core$Basics$not(model.isAnimating) ? _elm_lang$core$Maybe$Just(
						{
							ctor: '_Tuple2',
							_0: _elm_lang$core$Native_Utils.update(
								model,
								{
									isCompact: _p5._0.toCompact,
									isAnimating: _elm_lang$core$Basics$not(model.isSmallScreen) || _p5._0.fixedHeader
								}),
							_1: _elm_lang$core$Platform_Cmd$none
						}) : _elm_lang$core$Maybe$Nothing;
				default:
					return _elm_lang$core$Maybe$Just(
						_debois$elm_mdl$Material_Helpers$pure(
							_elm_lang$core$Native_Utils.update(
								model,
								{isAnimating: false})));
			}
		}
	});
var _debois$elm_mdl$Material_Layout$update = F2(
	function (msg, model) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			{ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none},
			A3(_debois$elm_mdl$Material_Layout$update$, _elm_lang$core$Basics$identity, msg, model));
	});
var _debois$elm_mdl$Material_Layout$pack = function (fwd) {
	return A4(
		_debois$elm_parts$Parts$pack1,
		_debois$elm_mdl$Material_Layout$update$,
		function (_) {
			return _.layout;
		},
		F2(
			function (x, c) {
				return _elm_lang$core$Native_Utils.update(
					c,
					{layout: x});
			}),
		fwd);
};
var _debois$elm_mdl$Material_Layout$ScrollPane = F2(
	function (a, b) {
		return {ctor: 'ScrollPane', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Layout$ScrollTab = function (a) {
	return {ctor: 'ScrollTab', _0: a};
};
var _debois$elm_mdl$Material_Layout$Resize = function (a) {
	return {ctor: 'Resize', _0: a};
};
var _debois$elm_mdl$Material_Layout$init = function () {
	var measureScreenSize = A3(
		_elm_lang$core$Task$perform,
		function (_p10) {
			return _debois$elm_mdl$Material_Layout$Resize(
				A2(_elm_lang$core$Debug$log, 'Can\'t get initial window dimensions. Guessing ', 1025));
		},
		_debois$elm_mdl$Material_Layout$Resize,
		_elm_lang$window$Window$width);
	return {ctor: '_Tuple2', _0: _debois$elm_mdl$Material_Layout$defaultModel, _1: measureScreenSize};
}();
var _debois$elm_mdl$Material_Layout$sub0 = function (lift) {
	return A2(
		_elm_lang$core$Platform_Cmd$map,
		_debois$elm_mdl$Material_Layout$pack(lift),
		_elm_lang$core$Basics$snd(_debois$elm_mdl$Material_Layout$init));
};
var _debois$elm_mdl$Material_Layout$subscriptions = function (model) {
	return _elm_lang$window$Window$resizes(
		function (_p11) {
			return _debois$elm_mdl$Material_Layout$Resize(
				function (_) {
					return _.width;
				}(_p11));
		});
};
var _debois$elm_mdl$Material_Layout$subs = function (lift) {
	return function (_p12) {
		return A2(
			_elm_lang$core$Platform_Sub$map,
			_debois$elm_mdl$Material_Layout$pack(lift),
			_debois$elm_mdl$Material_Layout$subscriptions(
				function (_) {
					return _.layout;
				}(_p12)));
	};
};
var _debois$elm_mdl$Material_Layout$ToggleDrawer = {ctor: 'ToggleDrawer'};
var _debois$elm_mdl$Material_Layout$drawerButton = F2(
	function (lift, isVisible) {
		return A2(
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[]),
			_elm_lang$core$Native_List.fromArray(
				[
					A2(
					_elm_lang$html$Html$div,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$classList(
							_elm_lang$core$Native_List.fromArray(
								[
									{ctor: '_Tuple2', _0: 'mdl-layout__drawer-button', _1: true}
								])),
							A2(
							_elm_lang$html$Html_Attributes$attribute,
							'aria-expanded',
							isVisible ? 'true' : 'false'),
							_elm_lang$html$Html_Attributes$tabindex(1),
							_elm_lang$html$Html_Events$onClick(
							lift(_debois$elm_mdl$Material_Layout$ToggleDrawer)),
							A3(
							_elm_lang$html$Html_Events$onWithOptions,
							'keydown',
							{stopPropagation: false, preventDefault: false},
							A2(
								_elm_lang$core$Json_Decode$map,
								function (_p13) {
									return lift(
										function (key) {
											var _p14 = key;
											switch (_p14) {
												case 32:
													return _debois$elm_mdl$Material_Layout$ToggleDrawer;
												case 13:
													return _debois$elm_mdl$Material_Layout$ToggleDrawer;
												default:
													return _debois$elm_mdl$Material_Layout$NOP;
											}
										}(_p13));
								},
								_elm_lang$html$Html_Events$keyCode))
						]),
					_elm_lang$core$Native_List.fromArray(
						[
							_debois$elm_mdl$Material_Icon$i('menu')
						]))
				]));
	});
var _debois$elm_mdl$Material_Layout$obfuscator = F2(
	function (lift, isVisible) {
		return A2(
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html_Attributes$classList(
					_elm_lang$core$Native_List.fromArray(
						[
							{ctor: '_Tuple2', _0: 'mdl-layout__obfuscator', _1: true},
							{ctor: '_Tuple2', _0: 'is-visible', _1: isVisible}
						])),
					_elm_lang$html$Html_Events$onClick(
					lift(_debois$elm_mdl$Material_Layout$ToggleDrawer))
				]),
			_elm_lang$core$Native_List.fromArray(
				[]));
	});
var _debois$elm_mdl$Material_Layout$toggleDrawer = function (lift) {
	return A2(_debois$elm_mdl$Material_Layout$pack, lift, _debois$elm_mdl$Material_Layout$ToggleDrawer);
};
var _debois$elm_mdl$Material_Layout$LinkProp = {ctor: 'LinkProp'};
var _debois$elm_mdl$Material_Layout$Waterfall = function (a) {
	return {ctor: 'Waterfall', _0: a};
};
var _debois$elm_mdl$Material_Layout$waterfall = function (b) {
	return _debois$elm_mdl$Material_Options$set(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{
					mode: _debois$elm_mdl$Material_Layout$Waterfall(b)
				});
		});
};
var _debois$elm_mdl$Material_Layout$Scrolling = {ctor: 'Scrolling'};
var _debois$elm_mdl$Material_Layout$scrolling = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{mode: _debois$elm_mdl$Material_Layout$Scrolling});
	});
var _debois$elm_mdl$Material_Layout$Seamed = {ctor: 'Seamed'};
var _debois$elm_mdl$Material_Layout$seamed = _debois$elm_mdl$Material_Options$set(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{mode: _debois$elm_mdl$Material_Layout$Seamed});
	});
var _debois$elm_mdl$Material_Layout$Standard = {ctor: 'Standard'};
var _debois$elm_mdl$Material_Layout$defaultConfig = {fixedHeader: false, fixedDrawer: false, fixedTabs: false, rippleTabs: true, mode: _debois$elm_mdl$Material_Layout$Standard, onSelectTab: _elm_lang$core$Maybe$Nothing, selectedTab: -1, moreTabs: false, transparentHeader: false};
var _debois$elm_mdl$Material_Layout$headerView = F4(
	function (lift, config, model, _p15) {
		var _p16 = _p15;
		var mode = function () {
			var _p17 = config.mode;
			switch (_p17.ctor) {
				case 'Standard':
					return _debois$elm_mdl$Material_Options$nop;
				case 'Scrolling':
					return _debois$elm_mdl$Material_Options$cs('mdl-layout__header--scroll');
				case 'Seamed':
					return _debois$elm_mdl$Material_Options$cs('mdl-layout__header--seamed');
				default:
					if (_p17._0 === true) {
						return _debois$elm_mdl$Material_Options$cs('mdl-layout__header--waterfall mdl-layout__header--waterfall-hide-top');
					} else {
						return _debois$elm_mdl$Material_Options$cs('mdl-layout__header--waterfall');
					}
			}
		}();
		return A3(
			_debois$elm_mdl$Material_Options$styled,
			_elm_lang$html$Html$header,
			_elm_lang$core$Native_List.fromArray(
				[
					_debois$elm_mdl$Material_Options$cs('mdl-layout__header'),
					A2(
					_debois$elm_mdl$Material_Options$when,
					_debois$elm_mdl$Material_Options$cs('is-casting-shadow'),
					_elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Standard) || (_debois$elm_mdl$Material_Layout$isWaterfall(config.mode) && model.isCompact)),
					A2(
					_debois$elm_mdl$Material_Options$when,
					_debois$elm_mdl$Material_Options$cs('is-animating'),
					model.isAnimating),
					A2(
					_debois$elm_mdl$Material_Options$when,
					_debois$elm_mdl$Material_Options$cs('is-compact'),
					model.isCompact),
					mode,
					A2(
					_debois$elm_mdl$Material_Options$when,
					_debois$elm_mdl$Material_Options$cs('mdl-layout__header--transparent'),
					config.transparentHeader),
					_debois$elm_mdl$Material_Options$attribute(
					_elm_lang$html$Html_Events$onClick(
						lift(
							_debois$elm_mdl$Material_Layout$TransitionHeader(
								{toCompact: false, fixedHeader: config.fixedHeader})))),
					_debois$elm_mdl$Material_Options$attribute(
					A2(
						_elm_lang$html$Html_Events$on,
						'transitionend',
						_elm_lang$core$Json_Decode$succeed(
							lift(_debois$elm_mdl$Material_Layout$TransitionEnd))))
				]),
			A2(
				_elm_lang$core$List$concatMap,
				function (x) {
					return x;
				},
				_elm_lang$core$Native_List.fromArray(
					[
						_debois$elm_mdl$Material_Layout$toList(_p16._0),
						_p16._1,
						_debois$elm_mdl$Material_Layout$toList(_p16._2)
					])));
	});
var _debois$elm_mdl$Material_Layout$Right = {ctor: 'Right'};
var _debois$elm_mdl$Material_Layout$Left = {ctor: 'Left'};
var _debois$elm_mdl$Material_Layout$tabsView = F4(
	function (lift, config, model, _p18) {
		var _p19 = _p18;
		var _p22 = _p19._1;
		var chevron = F2(
			function (direction, offset) {
				var dir = function () {
					var _p20 = direction;
					if (_p20.ctor === 'Left') {
						return 'left';
					} else {
						return 'right';
					}
				}();
				return A3(
					_debois$elm_mdl$Material_Options$styled,
					_elm_lang$html$Html$div,
					_elm_lang$core$Native_List.fromArray(
						[
							_debois$elm_mdl$Material_Options$cs('mdl-layout__tab-bar-button'),
							_debois$elm_mdl$Material_Options$cs(
							A2(
								_elm_lang$core$Basics_ops['++'],
								'mdl-layout__tab-bar-',
								A2(_elm_lang$core$Basics_ops['++'], dir, '-button'))),
							A2(
							_debois$elm_mdl$Material_Options$when,
							_debois$elm_mdl$Material_Options$cs('is-active'),
							(_elm_lang$core$Native_Utils.eq(direction, _debois$elm_mdl$Material_Layout$Left) && model.tabScrollState.canScrollLeft) || (_elm_lang$core$Native_Utils.eq(direction, _debois$elm_mdl$Material_Layout$Right) && model.tabScrollState.canScrollRight)),
							_debois$elm_mdl$Material_Options$many(_p22)
						]),
					_elm_lang$core$Native_List.fromArray(
						[
							A2(
							_debois$elm_mdl$Material_Icon$view,
							A2(_elm_lang$core$Basics_ops['++'], 'chevron_', dir),
							_elm_lang$core$Native_List.fromArray(
								[
									_debois$elm_mdl$Material_Icon$size24,
									_debois$elm_mdl$Material_Options_Internal$attribute(
									A2(
										_elm_lang$html$Html_Attributes$attribute,
										'onclick',
										A2(
											_elm_lang$core$Basics_ops['++'],
											'document.getElementsByClassName(\'mdl-layout__tab-bar\')[0].scrollLeft += ',
											_elm_lang$core$Basics$toString(offset))))
								]))
						]));
			});
		return A2(
			_debois$elm_mdl$Material_Options$div,
			_elm_lang$core$Native_List.fromArray(
				[
					_debois$elm_mdl$Material_Options$cs('mdl-layout__tab-bar-container')
				]),
			_elm_lang$core$Native_List.fromArray(
				[
					A2(chevron, _debois$elm_mdl$Material_Layout$Left, -100),
					A2(
					_debois$elm_mdl$Material_Options$div,
					_elm_lang$core$Native_List.fromArray(
						[
							_debois$elm_mdl$Material_Options$cs('mdl-layout__tab-bar'),
							A2(_debois$elm_mdl$Material_Options$css, 'position', 'relative'),
							A2(_debois$elm_mdl$Material_Options$css, 'scroll-behavior', 'smooth'),
							config.rippleTabs ? _debois$elm_mdl$Material_Options$many(
							_elm_lang$core$Native_List.fromArray(
								[
									_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect'),
									_debois$elm_mdl$Material_Options$cs('mds-js-ripple-effect--ignore-events')
								])) : _debois$elm_mdl$Material_Options$nop,
							_elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Standard) ? _debois$elm_mdl$Material_Options$cs('is-casting-shadow') : _debois$elm_mdl$Material_Options$nop,
							_debois$elm_mdl$Material_Options$many(_p22),
							_debois$elm_mdl$Material_Options_Internal$attribute(
							A2(
								_elm_lang$html$Html_Events$on,
								'scroll',
								_debois$elm_dom$DOM$target(
									A4(
										_elm_lang$core$Json_Decode$object3,
										F3(
											function (scrollWidth, clientWidth, scrollLeft) {
												return lift(
													_debois$elm_mdl$Material_Layout$ScrollTab(
														{
															canScrollLeft: _elm_lang$core$Native_Utils.cmp(scrollLeft, 0) > 0,
															canScrollRight: _elm_lang$core$Native_Utils.cmp(scrollWidth - clientWidth, scrollLeft + 1) > 0,
															width: _elm_lang$core$Maybe$Just(scrollWidth)
														}));
											}),
										A2(_elm_lang$core$Json_Decode_ops[':='], 'scrollWidth', _elm_lang$core$Json_Decode$float),
										A2(_elm_lang$core$Json_Decode_ops[':='], 'clientWidth', _elm_lang$core$Json_Decode$float),
										A2(_elm_lang$core$Json_Decode_ops[':='], 'scrollLeft', _elm_lang$core$Json_Decode$float)))))
						]),
					A2(
						_elm_lang$core$List$indexedMap,
						F2(
							function (tabIndex, tab) {
								return A3(
									_debois$elm_mdl$Material_Helpers$filter,
									_elm_lang$html$Html$a,
									_elm_lang$core$Native_List.fromArray(
										[
											_elm_lang$html$Html_Attributes$classList(
											_elm_lang$core$Native_List.fromArray(
												[
													{ctor: '_Tuple2', _0: 'mdl-layout__tab', _1: true},
													{
													ctor: '_Tuple2',
													_0: 'is-active',
													_1: _elm_lang$core$Native_Utils.eq(tabIndex, config.selectedTab)
												}
												])),
											A2(
											_elm_lang$core$Maybe$withDefault,
											_debois$elm_mdl$Material_Helpers$noAttr,
											A2(
												_elm_lang$core$Maybe$map,
												F2(
													function (x, y) {
														return y(x);
													})(tabIndex),
												config.onSelectTab))
										]),
									_elm_lang$core$Native_List.fromArray(
										[
											_elm_lang$core$Maybe$Just(tab),
											config.rippleTabs ? _elm_lang$core$Maybe$Just(
											A2(
												_elm_lang$html$Html_App$map,
												function (_p21) {
													return lift(
														A2(_debois$elm_mdl$Material_Layout$Ripple, tabIndex, _p21));
												},
												A2(
													_debois$elm_mdl$Material_Ripple$view,
													_elm_lang$core$Native_List.fromArray(
														[
															_elm_lang$html$Html_Attributes$class('mdl-layout__tab-ripple-container')
														]),
													A2(
														_elm_lang$core$Maybe$withDefault,
														_debois$elm_mdl$Material_Ripple$model,
														A2(_elm_lang$core$Dict$get, tabIndex, model.ripples))))) : _elm_lang$core$Maybe$Nothing
										]));
							}),
						_p19._0)),
					A2(chevron, _debois$elm_mdl$Material_Layout$Right, 100)
				]));
	});
var _debois$elm_mdl$Material_Layout$view = F4(
	function (lift, model, options, _p23) {
		var _p24 = _p23;
		var _p32 = _p24.tabs;
		var _p31 = _p24.header;
		var _p30 = _p24.drawer;
		var hasDrawer = !_elm_lang$core$Native_Utils.eq(
			_p30,
			_elm_lang$core$Native_List.fromArray(
				[]));
		var hasTabs = _elm_lang$core$Basics$not(
			_elm_lang$core$List$isEmpty(
				_elm_lang$core$Basics$fst(_p32)));
		var hasHeader = hasTabs || _elm_lang$core$Basics$not(
			_elm_lang$core$List$isEmpty(_p31));
		var summary = A2(_debois$elm_mdl$Material_Options$collect, _debois$elm_mdl$Material_Layout$defaultConfig, options);
		var config = summary.config;
		var drawerIsFixed = config.fixedDrawer && _elm_lang$core$Basics$not(model.isSmallScreen);
		var drawerIsVisible = model.isDrawerOpen && _elm_lang$core$Basics$not(drawerIsFixed);
		var _p25 = function () {
			var _p26 = {ctor: '_Tuple3', _0: _p30, _1: _p31, _2: config.fixedHeader};
			if ((_p26.ctor === '_Tuple3') && (_p26._0.ctor === '::')) {
				if ((_p26._1.ctor === '::') && (_p26._2 === true)) {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Maybe$Nothing,
						_1: _elm_lang$core$Maybe$Just(
							A2(_debois$elm_mdl$Material_Layout$drawerButton, lift, drawerIsVisible))
					};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Maybe$Just(
							A2(_debois$elm_mdl$Material_Layout$drawerButton, lift, drawerIsVisible)),
						_1: _elm_lang$core$Maybe$Nothing
					};
				}
			} else {
				return {ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: _elm_lang$core$Maybe$Nothing};
			}
		}();
		var contentDrawerButton = _p25._0;
		var headerDrawerButton = _p25._1;
		var tabsElems = _elm_lang$core$Basics$not(hasTabs) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
			A4(_debois$elm_mdl$Material_Layout$tabsView, lift, config, model, _p32));
		return A2(
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html_Attributes$classList(
					_elm_lang$core$Native_List.fromArray(
						[
							{ctor: '_Tuple2', _0: 'mdl-layout__container', _1: true},
							{
							ctor: '_Tuple2',
							_0: 'has-scrolling-header',
							_1: _elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Scrolling)
						}
						]))
				]),
			_elm_lang$core$Native_List.fromArray(
				[
					A3(
					_debois$elm_mdl$Material_Helpers$filter,
					_elm_lang$html$Html_Keyed$node('div'),
					A2(
						_elm_lang$core$List$filterMap,
						_elm_lang$core$Basics$identity,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$core$Maybe$Just(
								_elm_lang$html$Html_Attributes$classList(
									_elm_lang$core$Native_List.fromArray(
										[
											{ctor: '_Tuple2', _0: 'mdl-layout ', _1: true},
											{ctor: '_Tuple2', _0: 'is-upgraded', _1: true},
											{ctor: '_Tuple2', _0: 'is-small-screen', _1: model.isSmallScreen},
											{ctor: '_Tuple2', _0: 'has-drawer', _1: hasDrawer},
											{ctor: '_Tuple2', _0: 'has-tabs', _1: hasTabs},
											{ctor: '_Tuple2', _0: 'mdl-js-layout', _1: true},
											{ctor: '_Tuple2', _0: 'mdl-layout--fixed-drawer', _1: config.fixedDrawer && hasDrawer},
											{ctor: '_Tuple2', _0: 'mdl-layout--fixed-header', _1: config.fixedHeader && hasHeader},
											{ctor: '_Tuple2', _0: 'mdl-layout--fixed-tabs', _1: config.fixedTabs && hasTabs}
										]))),
								drawerIsVisible ? _elm_lang$core$Maybe$Just(
								A2(
									_elm_lang$html$Html_Events$on,
									'keydown',
									A2(
										_elm_lang$core$Json_Decode$map,
										function (_p27) {
											return lift(
												function (key) {
													return _elm_lang$core$Native_Utils.eq(key, 27) ? _debois$elm_mdl$Material_Layout$ToggleDrawer : _debois$elm_mdl$Material_Layout$NOP;
												}(_p27));
										},
										_elm_lang$html$Html_Events$keyCode))) : _elm_lang$core$Maybe$Nothing
							])),
					_elm_lang$core$Native_List.fromArray(
						[
							hasHeader ? _elm_lang$core$Maybe$Just(
							A2(
								F2(
									function (v0, v1) {
										return {ctor: '_Tuple2', _0: v0, _1: v1};
									}),
								'elm-mdl-header',
								A4(
									_debois$elm_mdl$Material_Layout$headerView,
									lift,
									config,
									model,
									{ctor: '_Tuple3', _0: headerDrawerButton, _1: _p31, _2: tabsElems}))) : _elm_lang$core$Maybe$Nothing,
							_elm_lang$core$Basics$not(hasDrawer) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
							{
								ctor: '_Tuple2',
								_0: 'elm-mdl-drawer',
								_1: A3(_debois$elm_mdl$Material_Layout$drawerView, lift, drawerIsVisible, _p30)
							}),
							_elm_lang$core$Basics$not(hasDrawer) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
							{
								ctor: '_Tuple2',
								_0: 'elm-mdl-obfuscator',
								_1: A2(_debois$elm_mdl$Material_Layout$obfuscator, lift, drawerIsVisible)
							}),
							A2(
							_elm_lang$core$Maybe$map,
							F2(
								function (v0, v1) {
									return {ctor: '_Tuple2', _0: v0, _1: v1};
								})('elm-drawer-button'),
							contentDrawerButton),
							_elm_lang$core$Maybe$Just(
							A2(
								F2(
									function (v0, v1) {
										return {ctor: '_Tuple2', _0: v0, _1: v1};
									}),
								_elm_lang$core$Basics$toString(config.selectedTab),
								A3(
									_debois$elm_mdl$Material_Options$styled,
									_elm_lang$html$Html$main$,
									_elm_lang$core$Native_List.fromArray(
										[
											_debois$elm_mdl$Material_Options$cs('mdl-layout__content'),
											A2(
											_debois$elm_mdl$Material_Options$when,
											A2(_debois$elm_mdl$Material_Options$css, 'overflow-y', 'visible'),
											_elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Scrolling) && config.fixedHeader),
											A2(
											_debois$elm_mdl$Material_Options$when,
											A2(_debois$elm_mdl$Material_Options$css, 'overflow-x', 'visible'),
											_elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Scrolling) && config.fixedHeader),
											A2(
											_debois$elm_mdl$Material_Options$when,
											A2(_debois$elm_mdl$Material_Options$css, 'overflow', 'visible'),
											_elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Scrolling) && config.fixedHeader),
											A2(
											_debois$elm_mdl$Material_Options$when,
											function (_p28) {
												return _debois$elm_mdl$Material_Options_Internal$attribute(
													A2(_elm_lang$html$Html_Events$on, 'scroll', _p28));
											}(
												A2(
													_elm_lang$core$Json_Decode$map,
													function (_p29) {
														return lift(
															A2(_debois$elm_mdl$Material_Layout$ScrollPane, config.fixedHeader, _p29));
													},
													_debois$elm_dom$DOM$target(_debois$elm_dom$DOM$scrollTop))),
											_debois$elm_mdl$Material_Layout$isWaterfall(config.mode))
										]),
									_p24.main)))
						]))
				]));
	});
var _debois$elm_mdl$Material_Layout$render = A4(
	_debois$elm_parts$Parts$create1,
	_debois$elm_mdl$Material_Layout$view,
	_debois$elm_mdl$Material_Layout$update$,
	function (_) {
		return _.layout;
	},
	F2(
		function (x, c) {
			return _elm_lang$core$Native_Utils.update(
				c,
				{layout: x});
		}));

var _debois$elm_mdl$Material_Toggles$group = function (s) {
	return _debois$elm_mdl$Material_Options$set(
		function (options) {
			return _elm_lang$core$Native_Utils.update(
				options,
				{
					group: _elm_lang$core$Maybe$Just(
						_elm_lang$html$Html_Attributes$name(s))
				});
		});
};
var _debois$elm_mdl$Material_Toggles$value = function (b) {
	return _debois$elm_mdl$Material_Options$set(
		function (options) {
			return _elm_lang$core$Native_Utils.update(
				options,
				{value: b});
		});
};
var _debois$elm_mdl$Material_Toggles$disabled = _debois$elm_mdl$Material_Options$set(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{isDisabled: true});
	});
var _debois$elm_mdl$Material_Toggles$ripple = _debois$elm_mdl$Material_Options$set(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{ripple: true});
	});
var _debois$elm_mdl$Material_Toggles$onClick = function (x) {
	return _debois$elm_mdl$Material_Options$set(
		function (options) {
			return _elm_lang$core$Native_Utils.update(
				options,
				{
					onClick: _elm_lang$core$Maybe$Just(
						A2(
							_elm_lang$html$Html_Events$on,
							'change',
							_elm_lang$core$Json_Decode$succeed(x)))
				});
		});
};
var _debois$elm_mdl$Material_Toggles$defaultConfig = {
	isDisabled: false,
	value: false,
	ripple: false,
	group: _elm_lang$core$Maybe$Nothing,
	onClick: _elm_lang$core$Maybe$Nothing,
	inner: _elm_lang$core$Native_List.fromArray(
		[])
};
var _debois$elm_mdl$Material_Toggles$defaultModel = {ripple: _debois$elm_mdl$Material_Ripple$model, isFocused: false};
var _debois$elm_mdl$Material_Toggles$Model = F2(
	function (a, b) {
		return {ripple: a, isFocused: b};
	});
var _debois$elm_mdl$Material_Toggles$Config = F6(
	function (a, b, c, d, e, f) {
		return {isDisabled: a, value: b, ripple: c, group: d, onClick: e, inner: f};
	});
var _debois$elm_mdl$Material_Toggles$SetFocus = function (a) {
	return {ctor: 'SetFocus', _0: a};
};
var _debois$elm_mdl$Material_Toggles$Ripple = function (a) {
	return {ctor: 'Ripple', _0: a};
};
var _debois$elm_mdl$Material_Toggles$update = F2(
	function (action, model) {
		var _p0 = action;
		if (_p0.ctor === 'Ripple') {
			return A2(
				_debois$elm_mdl$Material_Helpers$map2nd,
				_elm_lang$core$Platform_Cmd$map(_debois$elm_mdl$Material_Toggles$Ripple),
				A2(
					_debois$elm_mdl$Material_Helpers$map1st,
					function (r) {
						return _elm_lang$core$Native_Utils.update(
							model,
							{ripple: r});
					},
					A2(_debois$elm_mdl$Material_Ripple$update, _p0._0, model.ripple)));
		} else {
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Native_Utils.update(
					model,
					{isFocused: _p0._0}),
				_1: _elm_lang$core$Platform_Cmd$none
			};
		}
	});
var _debois$elm_mdl$Material_Toggles$render = function (view) {
	return A5(
		_debois$elm_parts$Parts$create,
		view,
		_debois$elm_parts$Parts$generalize(_debois$elm_mdl$Material_Toggles$update),
		function (_) {
			return _.toggles;
		},
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.update(
					y,
					{toggles: x});
			}),
		_debois$elm_mdl$Material_Toggles$defaultModel);
};
var _debois$elm_mdl$Material_Toggles$top = F5(
	function (lift, group, model, summary, elems) {
		var cfg = summary.config;
		return A5(
			_debois$elm_mdl$Material_Options$apply,
			summary,
			_elm_lang$html$Html$label,
			_elm_lang$core$Native_List.fromArray(
				[
					_debois$elm_mdl$Material_Options$cs(
					A2(_elm_lang$core$Basics_ops['++'], 'mdl-', group)),
					_debois$elm_mdl$Material_Options$cs(
					A2(_elm_lang$core$Basics_ops['++'], 'mdl-js-', group)),
					A2(
					_debois$elm_mdl$Material_Options$when,
					_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect'),
					cfg.ripple),
					A2(
					_debois$elm_mdl$Material_Options$when,
					_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect--ignore-events'),
					cfg.ripple),
					_debois$elm_mdl$Material_Options$cs('is-upgraded'),
					A2(
					_debois$elm_mdl$Material_Options$when,
					_debois$elm_mdl$Material_Options$cs('is-checked'),
					cfg.value),
					A2(
					_debois$elm_mdl$Material_Options$when,
					_debois$elm_mdl$Material_Options$cs('is-focused'),
					model.isFocused)
				]),
			_elm_lang$core$Native_List.fromArray(
				[
					_debois$elm_mdl$Material_Helpers$blurOn('mouseup'),
					_elm_lang$html$Html_Events$onFocus(
					lift(
						_debois$elm_mdl$Material_Toggles$SetFocus(true))),
					_elm_lang$html$Html_Events$onBlur(
					lift(
						_debois$elm_mdl$Material_Toggles$SetFocus(false))),
					A2(_elm_lang$core$Maybe$withDefault, _debois$elm_mdl$Material_Helpers$noAttr, cfg.onClick)
				]),
			_elm_lang$core$List$concat(
				_elm_lang$core$Native_List.fromArray(
					[
						elems,
						cfg.ripple ? _elm_lang$core$Native_List.fromArray(
						[
							A2(
							_elm_lang$html$Html_App$map,
							function (_p1) {
								return lift(
									_debois$elm_mdl$Material_Toggles$Ripple(_p1));
							},
							A2(
								_debois$elm_mdl$Material_Ripple$view,
								_elm_lang$core$Native_List.fromArray(
									[
										_elm_lang$html$Html_Attributes$class('mdl-switch__ripple-container mdl-js-ripple-effect mdl-ripple--center')
									]),
								model.ripple))
						]) : _elm_lang$core$Native_List.fromArray(
						[])
					])));
	});
var _debois$elm_mdl$Material_Toggles$viewCheckbox = F4(
	function (lift, model, config, elems) {
		var summary = A2(_debois$elm_mdl$Material_Options$collect, _debois$elm_mdl$Material_Toggles$defaultConfig, config);
		var cfg = summary.config;
		return A5(
			_debois$elm_mdl$Material_Toggles$top,
			lift,
			'checkbox',
			model,
			summary,
			_elm_lang$core$Native_List.fromArray(
				[
					A4(
					_debois$elm_mdl$Material_Options$styled$,
					_elm_lang$html$Html$input,
					_elm_lang$core$Native_List.fromArray(
						[
							_debois$elm_mdl$Material_Options$cs('mdl-checkbox__input'),
							_debois$elm_mdl$Material_Options$many(cfg.inner)
						]),
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$type$('checkbox'),
							_elm_lang$html$Html_Attributes$disabled(cfg.isDisabled),
							_elm_lang$html$Html_Attributes$checked(cfg.value)
						]),
					_elm_lang$core$Native_List.fromArray(
						[])),
					A2(
					_elm_lang$html$Html$span,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('mdl-checkbox__label')
						]),
					elems),
					A2(
					_elm_lang$html$Html$span,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('mdl-checkbox__focus-helper')
						]),
					_elm_lang$core$Native_List.fromArray(
						[])),
					A2(
					_elm_lang$html$Html$span,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('mdl-checkbox__box-outline')
						]),
					_elm_lang$core$Native_List.fromArray(
						[
							A2(
							_elm_lang$html$Html$span,
							_elm_lang$core$Native_List.fromArray(
								[
									_elm_lang$html$Html_Attributes$class('mdl-checkbox__tick-outline')
								]),
							_elm_lang$core$Native_List.fromArray(
								[]))
						]))
				]));
	});
var _debois$elm_mdl$Material_Toggles$checkbox = _debois$elm_mdl$Material_Toggles$render(_debois$elm_mdl$Material_Toggles$viewCheckbox);
var _debois$elm_mdl$Material_Toggles$viewSwitch = F4(
	function (lift, model, config, elems) {
		var summary = A2(_debois$elm_mdl$Material_Options$collect, _debois$elm_mdl$Material_Toggles$defaultConfig, config);
		var cfg = summary.config;
		return A5(
			_debois$elm_mdl$Material_Toggles$top,
			lift,
			'switch',
			model,
			summary,
			_elm_lang$core$Native_List.fromArray(
				[
					A4(
					_debois$elm_mdl$Material_Options$styled$,
					_elm_lang$html$Html$input,
					_elm_lang$core$Native_List.fromArray(
						[
							_debois$elm_mdl$Material_Options$cs('mdl-switch__input'),
							_debois$elm_mdl$Material_Options$many(cfg.inner)
						]),
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$type$('checkbox'),
							_elm_lang$html$Html_Attributes$disabled(cfg.isDisabled),
							_elm_lang$html$Html_Attributes$checked(cfg.value)
						]),
					_elm_lang$core$Native_List.fromArray(
						[])),
					A2(
					_elm_lang$html$Html$span,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('mdl-switch__label')
						]),
					elems),
					A2(
					_elm_lang$html$Html$div,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('mdl-switch__track')
						]),
					_elm_lang$core$Native_List.fromArray(
						[])),
					A2(
					_elm_lang$html$Html$div,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('mdl-switch__thumb')
						]),
					_elm_lang$core$Native_List.fromArray(
						[
							A2(
							_elm_lang$html$Html$span,
							_elm_lang$core$Native_List.fromArray(
								[
									_elm_lang$html$Html_Attributes$class('mdl-switch__focus-helper')
								]),
							_elm_lang$core$Native_List.fromArray(
								[]))
						]))
				]));
	});
var _debois$elm_mdl$Material_Toggles$switch = _debois$elm_mdl$Material_Toggles$render(_debois$elm_mdl$Material_Toggles$viewSwitch);
var _debois$elm_mdl$Material_Toggles$viewRadio = F4(
	function (lift, model, config, elems) {
		var summary = A2(_debois$elm_mdl$Material_Options$collect, _debois$elm_mdl$Material_Toggles$defaultConfig, config);
		var cfg = summary.config;
		return A5(
			_debois$elm_mdl$Material_Toggles$top,
			lift,
			'radio',
			model,
			summary,
			_elm_lang$core$Native_List.fromArray(
				[
					A4(
					_debois$elm_mdl$Material_Options$styled$,
					_elm_lang$html$Html$input,
					_elm_lang$core$Native_List.fromArray(
						[
							_debois$elm_mdl$Material_Options$cs('mdl-radio__button'),
							_debois$elm_mdl$Material_Options$many(cfg.inner)
						]),
					A2(
						_elm_lang$core$List$filterMap,
						_elm_lang$core$Basics$identity,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$core$Maybe$Just(
								_elm_lang$html$Html_Attributes$type$('radio')),
								_elm_lang$core$Maybe$Just(
								_elm_lang$html$Html_Attributes$disabled(cfg.isDisabled)),
								_elm_lang$core$Maybe$Just(
								_elm_lang$html$Html_Attributes$checked(cfg.value)),
								cfg.group
							])),
					_elm_lang$core$Native_List.fromArray(
						[])),
					A2(
					_elm_lang$html$Html$span,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('mdl-radio__label')
						]),
					elems),
					A2(
					_elm_lang$html$Html$span,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('mdl-radio__outer-circle')
						]),
					_elm_lang$core$Native_List.fromArray(
						[])),
					A2(
					_elm_lang$html$Html$span,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('mdl-radio__inner-circle')
						]),
					_elm_lang$core$Native_List.fromArray(
						[]))
				]));
	});
var _debois$elm_mdl$Material_Toggles$radio = _debois$elm_mdl$Material_Toggles$render(_debois$elm_mdl$Material_Toggles$viewRadio);

var _debois$elm_mdl$Material_Tooltip$set = F2(
	function (x, y) {
		return _elm_lang$core$Native_Utils.update(
			y,
			{tooltip: x});
	});
var _debois$elm_mdl$Material_Tooltip$container = function (elem) {
	return _debois$elm_mdl$Material_Options$set(
		function (options) {
			return _elm_lang$core$Native_Utils.update(
				options,
				{container: elem});
		});
};
var _debois$elm_mdl$Material_Tooltip$isTooltipClass = function (path) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		A2(_elm_lang$core$Json_Decode$at, path, _debois$elm_dom$DOM$className),
		function ($class) {
			return A2(_elm_lang$core$String$contains, 'mdl-tooltip', $class) ? _elm_lang$core$Json_Decode$succeed(true) : _elm_lang$core$Json_Decode$succeed(false);
		});
};
var _debois$elm_mdl$Material_Tooltip$sibling = function (d) {
	var valid = function (path) {
		return A2(
			_elm_lang$core$Json_Decode$andThen,
			_debois$elm_mdl$Material_Tooltip$isTooltipClass(path),
			function (res) {
				return res ? A2(_elm_lang$core$Json_Decode$at, path, d) : _elm_lang$core$Json_Decode$fail('');
			});
	};
	var createPath = function (depth) {
		var parents = A2(_elm_lang$core$List$repeat, depth, 'parentElement');
		return A2(
			_elm_lang$core$Basics_ops['++'],
			_elm_lang$core$Native_List.fromArray(
				['target']),
			A2(
				_elm_lang$core$Basics_ops['++'],
				parents,
				_elm_lang$core$Native_List.fromArray(
					['nextSibling'])));
	};
	var paths = A2(
		_elm_lang$core$List$map,
		createPath,
		_elm_lang$core$Native_List.range(0, 4));
	return _elm_lang$core$Json_Decode$oneOf(
		A2(_elm_lang$core$List$map, valid, paths));
};
var _debois$elm_mdl$Material_Tooltip$update = F2(
	function (action, model) {
		var _p0 = action;
		if (_p0.ctor === 'Enter') {
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Native_Utils.update(
					model,
					{isActive: true, domState: _p0._0}),
				_1: _elm_lang$core$Platform_Cmd$none
			};
		} else {
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Native_Utils.update(
					model,
					{isActive: false}),
				_1: _elm_lang$core$Platform_Cmd$none
			};
		}
	});
var _debois$elm_mdl$Material_Tooltip$calculatePos = F2(
	function (pos, domState) {
		var getValuesFor = F2(
			function (l, r) {
				return (_elm_lang$core$Native_Utils.cmp(l + r, 0) < 0) ? {ctor: '_Tuple2', _0: 0, _1: 0} : {ctor: '_Tuple2', _0: l, _1: r};
			});
		var offsetHeight = domState.offsetHeight;
		var marginTop = -1 * (offsetHeight / 2);
		var offsetWidth = domState.offsetWidth;
		var marginLeft = -1 * (offsetWidth / 2);
		var props = domState.rect;
		var left = props.left + (props.width / 2);
		var _p1 = A2(getValuesFor, left, marginLeft);
		var newLeft = _p1._0;
		var newMarginLeft = _p1._1;
		var top = props.top + (props.height / 2);
		var _p2 = A2(getValuesFor, top, marginTop);
		var newTop = _p2._0;
		var newMarginTop = _p2._1;
		var out = function () {
			var _p3 = pos;
			switch (_p3.ctor) {
				case 'Left':
					return {left: (props.left - offsetWidth) - 10, top: newTop, marginTop: newMarginTop, marginLeft: 0};
				case 'Right':
					return {left: (props.left + props.width) + 10, top: newTop, marginTop: newMarginTop, marginLeft: 0};
				case 'Top':
					return {left: newLeft, top: (props.top - offsetHeight) - 10, marginTop: 0, marginLeft: newMarginLeft};
				default:
					return {left: newLeft, top: (props.top + props.height) + 10, marginTop: 0, marginLeft: newMarginLeft};
			}
		}();
		return out;
	});
var _debois$elm_mdl$Material_Tooltip$defaultDOMState = {
	rect: {left: 0, top: 0, width: 0, height: 0},
	offsetWidth: 0,
	offsetHeight: 0
};
var _debois$elm_mdl$Material_Tooltip$defaultPos = {left: 0, top: 0, marginLeft: 0, marginTop: 0};
var _debois$elm_mdl$Material_Tooltip$defaultModel = {isActive: false, domState: _debois$elm_mdl$Material_Tooltip$defaultDOMState};
var _debois$elm_mdl$Material_Tooltip$pack = A4(
	_debois$elm_parts$Parts$pack,
	_debois$elm_parts$Parts$generalize(_debois$elm_mdl$Material_Tooltip$update),
	function (_) {
		return _.tooltip;
	},
	_debois$elm_mdl$Material_Tooltip$set,
	_debois$elm_mdl$Material_Tooltip$defaultModel);
var _debois$elm_mdl$Material_Tooltip$Model = F2(
	function (a, b) {
		return {isActive: a, domState: b};
	});
var _debois$elm_mdl$Material_Tooltip$Pos = F4(
	function (a, b, c, d) {
		return {left: a, top: b, marginLeft: c, marginTop: d};
	});
var _debois$elm_mdl$Material_Tooltip$DOMState = F3(
	function (a, b, c) {
		return {rect: a, offsetWidth: b, offsetHeight: c};
	});
var _debois$elm_mdl$Material_Tooltip$stateDecoder = A4(
	_elm_lang$core$Json_Decode$object3,
	_debois$elm_mdl$Material_Tooltip$DOMState,
	_debois$elm_dom$DOM$target(_debois$elm_dom$DOM$boundingClientRect),
	_debois$elm_mdl$Material_Tooltip$sibling(_debois$elm_dom$DOM$offsetWidth),
	_debois$elm_mdl$Material_Tooltip$sibling(_debois$elm_dom$DOM$offsetHeight));
var _debois$elm_mdl$Material_Tooltip$Config = F3(
	function (a, b, c) {
		return {size: a, position: b, container: c};
	});
var _debois$elm_mdl$Material_Tooltip$Leave = {ctor: 'Leave'};
var _debois$elm_mdl$Material_Tooltip$onMouseLeave = F2(
	function (lift, idx) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'mouseleave',
			_elm_lang$core$Json_Decode$succeed(
				A3(_debois$elm_mdl$Material_Tooltip$pack, lift, idx, _debois$elm_mdl$Material_Tooltip$Leave)));
	});
var _debois$elm_mdl$Material_Tooltip$onLeave = function (lift) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseleave',
		_elm_lang$core$Json_Decode$succeed(
			lift(_debois$elm_mdl$Material_Tooltip$Leave)));
};
var _debois$elm_mdl$Material_Tooltip$Enter = function (a) {
	return {ctor: 'Enter', _0: a};
};
var _debois$elm_mdl$Material_Tooltip$onMouseEnter = F2(
	function (lift, idx) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'mouseenter',
			A2(
				_elm_lang$core$Json_Decode$map,
				function (_p4) {
					return A3(
						_debois$elm_mdl$Material_Tooltip$pack,
						lift,
						idx,
						_debois$elm_mdl$Material_Tooltip$Enter(_p4));
				},
				_debois$elm_mdl$Material_Tooltip$stateDecoder));
	});
var _debois$elm_mdl$Material_Tooltip$attach = F2(
	function (lift, index) {
		return _debois$elm_mdl$Material_Options$many(
			_elm_lang$core$Native_List.fromArray(
				[
					_debois$elm_mdl$Material_Options_Internal$attribute(
					A2(_debois$elm_mdl$Material_Tooltip$onMouseEnter, lift, index)),
					_debois$elm_mdl$Material_Options_Internal$attribute(
					A2(_debois$elm_mdl$Material_Tooltip$onMouseLeave, lift, index))
				]));
	});
var _debois$elm_mdl$Material_Tooltip$onEnter = function (lift) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseenter',
		A2(
			_elm_lang$core$Json_Decode$map,
			function (_p5) {
				return lift(
					_debois$elm_mdl$Material_Tooltip$Enter(_p5));
			},
			_debois$elm_mdl$Material_Tooltip$stateDecoder));
};
var _debois$elm_mdl$Material_Tooltip$Large = {ctor: 'Large'};
var _debois$elm_mdl$Material_Tooltip$large = _debois$elm_mdl$Material_Options$set(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{size: _debois$elm_mdl$Material_Tooltip$Large});
	});
var _debois$elm_mdl$Material_Tooltip$Default = {ctor: 'Default'};
var _debois$elm_mdl$Material_Tooltip$Bottom = {ctor: 'Bottom'};
var _debois$elm_mdl$Material_Tooltip$defaultConfig = {size: _debois$elm_mdl$Material_Tooltip$Default, position: _debois$elm_mdl$Material_Tooltip$Bottom, container: _elm_lang$html$Html$div};
var _debois$elm_mdl$Material_Tooltip$view = F4(
	function (lift, model, options, content) {
		var px = function (f) {
			return A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(f),
				'px');
		};
		var summary = A2(_debois$elm_mdl$Material_Options$collect, _debois$elm_mdl$Material_Tooltip$defaultConfig, options);
		var config = summary.config;
		var pos = model.isActive ? A2(_debois$elm_mdl$Material_Tooltip$calculatePos, config.position, model.domState) : _debois$elm_mdl$Material_Tooltip$defaultPos;
		return A3(
			_debois$elm_mdl$Material_Options$styled,
			config.container,
			_elm_lang$core$Native_List.fromArray(
				[
					_debois$elm_mdl$Material_Options$cs('mdl-tooltip'),
					A2(
					_debois$elm_mdl$Material_Options$when,
					_debois$elm_mdl$Material_Options$cs('is-active'),
					model.isActive),
					A2(
					_debois$elm_mdl$Material_Options$when,
					_debois$elm_mdl$Material_Options$cs('mdl-tooltip--large'),
					_elm_lang$core$Native_Utils.eq(config.size, _debois$elm_mdl$Material_Tooltip$Large)),
					A2(
					_debois$elm_mdl$Material_Options$when,
					A2(
						_debois$elm_mdl$Material_Options$css,
						'left',
						px(pos.left)),
					model.isActive),
					A2(
					_debois$elm_mdl$Material_Options$when,
					A2(
						_debois$elm_mdl$Material_Options$css,
						'margin-left',
						px(pos.marginLeft)),
					model.isActive),
					A2(
					_debois$elm_mdl$Material_Options$when,
					A2(
						_debois$elm_mdl$Material_Options$css,
						'top',
						px(pos.top)),
					model.isActive),
					A2(
					_debois$elm_mdl$Material_Options$when,
					A2(
						_debois$elm_mdl$Material_Options$css,
						'margin-top',
						px(pos.marginTop)),
					model.isActive)
				]),
			content);
	});
var _debois$elm_mdl$Material_Tooltip$render = A5(
	_debois$elm_parts$Parts$create,
	_debois$elm_mdl$Material_Tooltip$view,
	_debois$elm_parts$Parts$generalize(_debois$elm_mdl$Material_Tooltip$update),
	function (_) {
		return _.tooltip;
	},
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.update(
				y,
				{tooltip: x});
		}),
	_debois$elm_mdl$Material_Tooltip$defaultModel);
var _debois$elm_mdl$Material_Tooltip$bottom = _debois$elm_mdl$Material_Options$set(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{position: _debois$elm_mdl$Material_Tooltip$Bottom});
	});
var _debois$elm_mdl$Material_Tooltip$Top = {ctor: 'Top'};
var _debois$elm_mdl$Material_Tooltip$top = _debois$elm_mdl$Material_Options$set(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{position: _debois$elm_mdl$Material_Tooltip$Top});
	});
var _debois$elm_mdl$Material_Tooltip$Right = {ctor: 'Right'};
var _debois$elm_mdl$Material_Tooltip$right = _debois$elm_mdl$Material_Options$set(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{position: _debois$elm_mdl$Material_Tooltip$Right});
	});
var _debois$elm_mdl$Material_Tooltip$Left = {ctor: 'Left'};
var _debois$elm_mdl$Material_Tooltip$left = _debois$elm_mdl$Material_Options$set(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{position: _debois$elm_mdl$Material_Tooltip$Left});
	});

var _debois$elm_mdl$Material_Tabs$activeTab = function (k) {
	return _debois$elm_mdl$Material_Options$set(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{activeTab: k});
		});
};
var _debois$elm_mdl$Material_Tabs$onSelectTab = function (k) {
	return _debois$elm_mdl$Material_Options$set(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{
					onSelectTab: _elm_lang$core$Maybe$Just(k)
				});
		});
};
var _debois$elm_mdl$Material_Tabs$ripple = _debois$elm_mdl$Material_Options$set(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{ripple: true});
	});
var _debois$elm_mdl$Material_Tabs$defaultConfig = {ripple: false, onSelectTab: _elm_lang$core$Maybe$Nothing, activeTab: 0};
var _debois$elm_mdl$Material_Tabs$defaultModel = {ripples: _elm_lang$core$Dict$empty};
var _debois$elm_mdl$Material_Tabs$Model = function (a) {
	return {ripples: a};
};
var _debois$elm_mdl$Material_Tabs$Config = F3(
	function (a, b, c) {
		return {ripple: a, onSelectTab: b, activeTab: c};
	});
var _debois$elm_mdl$Material_Tabs$Ripple = F2(
	function (a, b) {
		return {ctor: 'Ripple', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Tabs$update = F2(
	function (action, model) {
		var _p0 = action;
		var _p2 = _p0._0;
		var _p1 = A2(
			_debois$elm_mdl$Material_Ripple$update,
			_p0._1,
			A2(
				_elm_lang$core$Maybe$withDefault,
				_debois$elm_mdl$Material_Ripple$model,
				A2(_elm_lang$core$Dict$get, _p2, model.ripples)));
		var ripple$ = _p1._0;
		var cmd = _p1._1;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Native_Utils.update(
				model,
				{
					ripples: A3(_elm_lang$core$Dict$insert, _p2, ripple$, model.ripples)
				}),
			_1: A2(
				_elm_lang$core$Platform_Cmd$map,
				_debois$elm_mdl$Material_Tabs$Ripple(_p2),
				cmd)
		};
	});
var _debois$elm_mdl$Material_Tabs$view = F5(
	function (lift, model, options, tabs, tabContent) {
		var wrapContent = A2(
			_elm_lang$html$Html_Keyed$node,
			'div',
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html_Attributes$classList(
					_elm_lang$core$Native_List.fromArray(
						[
							{ctor: '_Tuple2', _0: 'mdl-tab__panel', _1: true},
							{ctor: '_Tuple2', _0: 'is-active', _1: true}
						]))
				]));
		var summary = A2(_debois$elm_mdl$Material_Options$collect, _debois$elm_mdl$Material_Tabs$defaultConfig, options);
		var config = summary.config;
		var unwrapLabel = F2(
			function (tabIdx, _p3) {
				var _p4 = _p3;
				var _p6 = _p4._0._1;
				return A3(
					_debois$elm_mdl$Material_Options$styled,
					_elm_lang$html$Html$a,
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Native_List.fromArray(
							[
								_debois$elm_mdl$Material_Options$cs('mdl-tabs__tab'),
								A2(
								_debois$elm_mdl$Material_Options$when,
								_debois$elm_mdl$Material_Options$cs('is-active'),
								_elm_lang$core$Native_Utils.eq(tabIdx, config.activeTab)),
								A2(
								_elm_lang$core$Maybe$withDefault,
								_debois$elm_mdl$Material_Options$nop,
								A2(
									_elm_lang$core$Maybe$map,
									function (t) {
										return _debois$elm_mdl$Material_Options_Internal$attribute(
											_elm_lang$html$Html_Events$onClick(
												t(tabIdx)));
									},
									config.onSelectTab))
							]),
						_p4._0._0),
					config.ripple ? _elm_lang$core$List$concat(
						_elm_lang$core$Native_List.fromArray(
							[
								_p6,
								_elm_lang$core$Native_List.fromArray(
								[
									A2(
									_elm_lang$html$Html_App$map,
									function (_p5) {
										return lift(
											A2(_debois$elm_mdl$Material_Tabs$Ripple, tabIdx, _p5));
									},
									A2(
										_debois$elm_mdl$Material_Ripple$view,
										_elm_lang$core$Native_List.fromArray(
											[
												_elm_lang$html$Html_Attributes$classList(
												_elm_lang$core$Native_List.fromArray(
													[
														{ctor: '_Tuple2', _0: 'mdl-tabs__ripple-container', _1: true},
														{ctor: '_Tuple2', _0: 'mdl-tabs__ripple-js-effect', _1: true}
													]))
											]),
										A2(
											_elm_lang$core$Maybe$withDefault,
											_debois$elm_mdl$Material_Ripple$model,
											A2(_elm_lang$core$Dict$get, tabIdx, model.ripples))))
								])
							])) : _p6);
			});
		var links = A3(
			_debois$elm_mdl$Material_Options$styled,
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[
					_debois$elm_mdl$Material_Options$cs('mdl-tabs__tab-bar')
				]),
			A2(_elm_lang$core$List$indexedMap, unwrapLabel, tabs));
		return A5(
			_debois$elm_mdl$Material_Options$apply,
			summary,
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[
					_debois$elm_mdl$Material_Options$cs('mdl-tabs'),
					_debois$elm_mdl$Material_Options$cs('mdl-js-tabs'),
					_debois$elm_mdl$Material_Options$cs('is-upgraded'),
					A2(
					_debois$elm_mdl$Material_Options$when,
					_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect'),
					config.ripple),
					A2(
					_debois$elm_mdl$Material_Options$when,
					_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect--ignore-events'),
					config.ripple)
				]),
			_elm_lang$core$Native_List.fromArray(
				[]),
			_elm_lang$core$Native_List.fromArray(
				[
					links,
					wrapContent(
					_elm_lang$core$Native_List.fromArray(
						[
							{
							ctor: '_Tuple2',
							_0: _elm_lang$core$Basics$toString(config.activeTab),
							_1: A2(
								_elm_lang$html$Html$div,
								_elm_lang$core$Native_List.fromArray(
									[]),
								tabContent)
						}
						]))
				]));
	});
var _debois$elm_mdl$Material_Tabs$render = A5(
	_debois$elm_parts$Parts$create,
	_debois$elm_mdl$Material_Tabs$view,
	_debois$elm_parts$Parts$generalize(_debois$elm_mdl$Material_Tabs$update),
	function (_) {
		return _.tabs;
	},
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.update(
				y,
				{tabs: x});
		}),
	_debois$elm_mdl$Material_Tabs$defaultModel);
var _debois$elm_mdl$Material_Tabs$Label = function (a) {
	return {ctor: 'Label', _0: a};
};
var _debois$elm_mdl$Material_Tabs$label = F2(
	function (p, c) {
		return _debois$elm_mdl$Material_Tabs$Label(
			{ctor: '_Tuple2', _0: p, _1: c});
	});
var _debois$elm_mdl$Material_Tabs$textLabel = F2(
	function (p, c) {
		return A2(
			_debois$elm_mdl$Material_Tabs$label,
			p,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html$text(c)
				]));
	});

var _debois$elm_mdl$Material$init = function (lift) {
	return _debois$elm_mdl$Material_Layout$sub0(lift);
};
var _debois$elm_mdl$Material$subscriptions = F2(
	function (lift, model) {
		return _elm_lang$core$Platform_Sub$batch(
			_elm_lang$core$Native_List.fromArray(
				[
					A2(_debois$elm_mdl$Material_Layout$subs, lift, model.mdl),
					A2(_debois$elm_mdl$Material_Menu$subs, lift, model.mdl)
				]));
	});
var _debois$elm_mdl$Material$update = F2(
	function (msg, model) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			{ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none},
			A2(
				_elm_lang$core$Maybe$map,
				_debois$elm_mdl$Material_Helpers$map1st(
					function (mdl) {
						return _elm_lang$core$Native_Utils.update(
							model,
							{mdl: mdl});
					}),
				A2(_debois$elm_parts$Parts$update$, msg, model.mdl)));
	});
var _debois$elm_mdl$Material$model = {button: _elm_lang$core$Dict$empty, textfield: _elm_lang$core$Dict$empty, menu: _elm_lang$core$Dict$empty, snackbar: _elm_lang$core$Maybe$Nothing, layout: _debois$elm_mdl$Material_Layout$defaultModel, toggles: _elm_lang$core$Dict$empty, tooltip: _elm_lang$core$Dict$empty, tabs: _elm_lang$core$Dict$empty};
var _debois$elm_mdl$Material$Model = F8(
	function (a, b, c, d, e, f, g, h) {
		return {button: a, textfield: b, menu: c, snackbar: d, layout: e, toggles: f, tooltip: g, tabs: h};
	});

var _debois$elm_mdl$Material_Grid$clip = F3(
	function (lower, upper, k) {
		return A2(
			_elm_lang$core$Basics$max,
			lower,
			A2(_elm_lang$core$Basics$min, k, upper));
	});
var _debois$elm_mdl$Material_Grid$stretch = _debois$elm_mdl$Material_Options$cs('mdl-cell--stretch');
var _debois$elm_mdl$Material_Grid$align = function (a) {
	var _p0 = a;
	switch (_p0.ctor) {
		case 'Top':
			return _debois$elm_mdl$Material_Options$cs('mdl-cell--top');
		case 'Middle':
			return _debois$elm_mdl$Material_Options$cs('mdl-cell--middle');
		default:
			return _debois$elm_mdl$Material_Options$cs('mdl-cell--bottom');
	}
};
var _debois$elm_mdl$Material_Grid$suffix = function (device) {
	var _p1 = device;
	switch (_p1.ctor) {
		case 'All':
			return '';
		case 'Desktop':
			return '-desktop';
		case 'Tablet':
			return '-tablet';
		default:
			return '-phone';
	}
};
var _debois$elm_mdl$Material_Grid$size = F2(
	function (device, k) {
		var c = function () {
			var _p2 = device;
			switch (_p2.ctor) {
				case 'All':
					return A3(_debois$elm_mdl$Material_Grid$clip, 1, 12, k);
				case 'Desktop':
					return A3(_debois$elm_mdl$Material_Grid$clip, 1, 12, k);
				case 'Tablet':
					return A3(_debois$elm_mdl$Material_Grid$clip, 1, 8, k);
				default:
					return A3(_debois$elm_mdl$Material_Grid$clip, 1, 4, k);
			}
		}();
		return _debois$elm_mdl$Material_Options$cs(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'mdl-cell--',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(c),
					A2(
						_elm_lang$core$Basics_ops['++'],
						'-col',
						_debois$elm_mdl$Material_Grid$suffix(device)))));
	});
var _debois$elm_mdl$Material_Grid$offset = F2(
	function (device, k) {
		var c = function () {
			var _p3 = device;
			switch (_p3.ctor) {
				case 'All':
					return A3(_debois$elm_mdl$Material_Grid$clip, 1, 11, k);
				case 'Desktop':
					return A3(_debois$elm_mdl$Material_Grid$clip, 1, 11, k);
				case 'Tablet':
					return A3(_debois$elm_mdl$Material_Grid$clip, 1, 7, k);
				default:
					return A3(_debois$elm_mdl$Material_Grid$clip, 1, 3, k);
			}
		}();
		return _debois$elm_mdl$Material_Options$cs(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'mdl-cell--',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(c),
					A2(
						_elm_lang$core$Basics_ops['++'],
						'-offset',
						_debois$elm_mdl$Material_Grid$suffix(device)))));
	});
var _debois$elm_mdl$Material_Grid$hide = function (device) {
	return _debois$elm_mdl$Material_Options$cs(
		function () {
			var _p4 = device;
			if (_p4.ctor === 'All') {
				return '';
			} else {
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'mdl-cell--hide-',
					_debois$elm_mdl$Material_Grid$suffix(device));
			}
		}());
};
var _debois$elm_mdl$Material_Grid$order = F2(
	function (device, n) {
		return _debois$elm_mdl$Material_Options$cs(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'mdl-cell--order-',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(
						A3(_debois$elm_mdl$Material_Grid$clip, 1, 12, n)),
					_debois$elm_mdl$Material_Grid$suffix(device))));
	});
var _debois$elm_mdl$Material_Grid$grid = F2(
	function (styling, cells) {
		return A2(
			_debois$elm_mdl$Material_Options$div,
			A2(
				_elm_lang$core$List_ops['::'],
				_debois$elm_mdl$Material_Options$cs('mdl-grid'),
				styling),
			A2(
				_elm_lang$core$List$map,
				function (_p5) {
					var _p6 = _p5;
					return _p6._0;
				},
				cells));
	});
var _debois$elm_mdl$Material_Grid$maxWidth = function (w) {
	return A2(_debois$elm_mdl$Material_Options$css, 'max-width', w);
};
var _debois$elm_mdl$Material_Grid$noSpacing = _debois$elm_mdl$Material_Options$cs('mdl-grid--no-spacing');
var _debois$elm_mdl$Material_Grid$Phone = {ctor: 'Phone'};
var _debois$elm_mdl$Material_Grid$Tablet = {ctor: 'Tablet'};
var _debois$elm_mdl$Material_Grid$Desktop = {ctor: 'Desktop'};
var _debois$elm_mdl$Material_Grid$All = {ctor: 'All'};
var _debois$elm_mdl$Material_Grid$Cell = function (a) {
	return {ctor: 'Cell', _0: a};
};
var _debois$elm_mdl$Material_Grid$cell = F2(
	function (styling, elms) {
		return _debois$elm_mdl$Material_Grid$Cell(
			A2(
				_debois$elm_mdl$Material_Options$div,
				A2(
					_elm_lang$core$List_ops['::'],
					_debois$elm_mdl$Material_Options$cs('mdl-cell'),
					styling),
				elms));
	});
var _debois$elm_mdl$Material_Grid$Bottom = {ctor: 'Bottom'};
var _debois$elm_mdl$Material_Grid$Middle = {ctor: 'Middle'};
var _debois$elm_mdl$Material_Grid$Top = {ctor: 'Top'};

var _elm_lang$core$Native_Bitwise = function() {

return {
	and: F2(function and(a, b) { return a & b; }),
	or: F2(function or(a, b) { return a | b; }),
	xor: F2(function xor(a, b) { return a ^ b; }),
	complement: function complement(a) { return ~a; },
	shiftLeft: F2(function sll(a, offset) { return a << offset; }),
	shiftRightArithmatic: F2(function sra(a, offset) { return a >> offset; }),
	shiftRightLogical: F2(function srl(a, offset) { return a >>> offset; })
};

}();

var _elm_lang$core$Bitwise$shiftRightLogical = _elm_lang$core$Native_Bitwise.shiftRightLogical;
var _elm_lang$core$Bitwise$shiftRight = _elm_lang$core$Native_Bitwise.shiftRightArithmatic;
var _elm_lang$core$Bitwise$shiftLeft = _elm_lang$core$Native_Bitwise.shiftLeft;
var _elm_lang$core$Bitwise$complement = _elm_lang$core$Native_Bitwise.complement;
var _elm_lang$core$Bitwise$xor = _elm_lang$core$Native_Bitwise.xor;
var _elm_lang$core$Bitwise$or = _elm_lang$core$Native_Bitwise.or;
var _elm_lang$core$Bitwise$and = _elm_lang$core$Native_Bitwise.and;

var _elm_lang$svg$Svg$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$svg$Svg$svgNamespace = A2(
	_elm_lang$virtual_dom$VirtualDom$property,
	'namespace',
	_elm_lang$core$Json_Encode$string('http://www.w3.org/2000/svg'));
var _elm_lang$svg$Svg$node = F3(
	function (name, attributes, children) {
		return A3(
			_elm_lang$virtual_dom$VirtualDom$node,
			name,
			A2(_elm_lang$core$List_ops['::'], _elm_lang$svg$Svg$svgNamespace, attributes),
			children);
	});
var _elm_lang$svg$Svg$svg = _elm_lang$svg$Svg$node('svg');
var _elm_lang$svg$Svg$foreignObject = _elm_lang$svg$Svg$node('foreignObject');
var _elm_lang$svg$Svg$animate = _elm_lang$svg$Svg$node('animate');
var _elm_lang$svg$Svg$animateColor = _elm_lang$svg$Svg$node('animateColor');
var _elm_lang$svg$Svg$animateMotion = _elm_lang$svg$Svg$node('animateMotion');
var _elm_lang$svg$Svg$animateTransform = _elm_lang$svg$Svg$node('animateTransform');
var _elm_lang$svg$Svg$mpath = _elm_lang$svg$Svg$node('mpath');
var _elm_lang$svg$Svg$set = _elm_lang$svg$Svg$node('set');
var _elm_lang$svg$Svg$a = _elm_lang$svg$Svg$node('a');
var _elm_lang$svg$Svg$defs = _elm_lang$svg$Svg$node('defs');
var _elm_lang$svg$Svg$g = _elm_lang$svg$Svg$node('g');
var _elm_lang$svg$Svg$marker = _elm_lang$svg$Svg$node('marker');
var _elm_lang$svg$Svg$mask = _elm_lang$svg$Svg$node('mask');
var _elm_lang$svg$Svg$missingGlyph = _elm_lang$svg$Svg$node('missingGlyph');
var _elm_lang$svg$Svg$pattern = _elm_lang$svg$Svg$node('pattern');
var _elm_lang$svg$Svg$switch = _elm_lang$svg$Svg$node('switch');
var _elm_lang$svg$Svg$symbol = _elm_lang$svg$Svg$node('symbol');
var _elm_lang$svg$Svg$desc = _elm_lang$svg$Svg$node('desc');
var _elm_lang$svg$Svg$metadata = _elm_lang$svg$Svg$node('metadata');
var _elm_lang$svg$Svg$title = _elm_lang$svg$Svg$node('title');
var _elm_lang$svg$Svg$feBlend = _elm_lang$svg$Svg$node('feBlend');
var _elm_lang$svg$Svg$feColorMatrix = _elm_lang$svg$Svg$node('feColorMatrix');
var _elm_lang$svg$Svg$feComponentTransfer = _elm_lang$svg$Svg$node('feComponentTransfer');
var _elm_lang$svg$Svg$feComposite = _elm_lang$svg$Svg$node('feComposite');
var _elm_lang$svg$Svg$feConvolveMatrix = _elm_lang$svg$Svg$node('feConvolveMatrix');
var _elm_lang$svg$Svg$feDiffuseLighting = _elm_lang$svg$Svg$node('feDiffuseLighting');
var _elm_lang$svg$Svg$feDisplacementMap = _elm_lang$svg$Svg$node('feDisplacementMap');
var _elm_lang$svg$Svg$feFlood = _elm_lang$svg$Svg$node('feFlood');
var _elm_lang$svg$Svg$feFuncA = _elm_lang$svg$Svg$node('feFuncA');
var _elm_lang$svg$Svg$feFuncB = _elm_lang$svg$Svg$node('feFuncB');
var _elm_lang$svg$Svg$feFuncG = _elm_lang$svg$Svg$node('feFuncG');
var _elm_lang$svg$Svg$feFuncR = _elm_lang$svg$Svg$node('feFuncR');
var _elm_lang$svg$Svg$feGaussianBlur = _elm_lang$svg$Svg$node('feGaussianBlur');
var _elm_lang$svg$Svg$feImage = _elm_lang$svg$Svg$node('feImage');
var _elm_lang$svg$Svg$feMerge = _elm_lang$svg$Svg$node('feMerge');
var _elm_lang$svg$Svg$feMergeNode = _elm_lang$svg$Svg$node('feMergeNode');
var _elm_lang$svg$Svg$feMorphology = _elm_lang$svg$Svg$node('feMorphology');
var _elm_lang$svg$Svg$feOffset = _elm_lang$svg$Svg$node('feOffset');
var _elm_lang$svg$Svg$feSpecularLighting = _elm_lang$svg$Svg$node('feSpecularLighting');
var _elm_lang$svg$Svg$feTile = _elm_lang$svg$Svg$node('feTile');
var _elm_lang$svg$Svg$feTurbulence = _elm_lang$svg$Svg$node('feTurbulence');
var _elm_lang$svg$Svg$font = _elm_lang$svg$Svg$node('font');
var _elm_lang$svg$Svg$fontFace = _elm_lang$svg$Svg$node('fontFace');
var _elm_lang$svg$Svg$fontFaceFormat = _elm_lang$svg$Svg$node('fontFaceFormat');
var _elm_lang$svg$Svg$fontFaceName = _elm_lang$svg$Svg$node('fontFaceName');
var _elm_lang$svg$Svg$fontFaceSrc = _elm_lang$svg$Svg$node('fontFaceSrc');
var _elm_lang$svg$Svg$fontFaceUri = _elm_lang$svg$Svg$node('fontFaceUri');
var _elm_lang$svg$Svg$hkern = _elm_lang$svg$Svg$node('hkern');
var _elm_lang$svg$Svg$vkern = _elm_lang$svg$Svg$node('vkern');
var _elm_lang$svg$Svg$linearGradient = _elm_lang$svg$Svg$node('linearGradient');
var _elm_lang$svg$Svg$radialGradient = _elm_lang$svg$Svg$node('radialGradient');
var _elm_lang$svg$Svg$stop = _elm_lang$svg$Svg$node('stop');
var _elm_lang$svg$Svg$circle = _elm_lang$svg$Svg$node('circle');
var _elm_lang$svg$Svg$ellipse = _elm_lang$svg$Svg$node('ellipse');
var _elm_lang$svg$Svg$image = _elm_lang$svg$Svg$node('image');
var _elm_lang$svg$Svg$line = _elm_lang$svg$Svg$node('line');
var _elm_lang$svg$Svg$path = _elm_lang$svg$Svg$node('path');
var _elm_lang$svg$Svg$polygon = _elm_lang$svg$Svg$node('polygon');
var _elm_lang$svg$Svg$polyline = _elm_lang$svg$Svg$node('polyline');
var _elm_lang$svg$Svg$rect = _elm_lang$svg$Svg$node('rect');
var _elm_lang$svg$Svg$use = _elm_lang$svg$Svg$node('use');
var _elm_lang$svg$Svg$feDistantLight = _elm_lang$svg$Svg$node('feDistantLight');
var _elm_lang$svg$Svg$fePointLight = _elm_lang$svg$Svg$node('fePointLight');
var _elm_lang$svg$Svg$feSpotLight = _elm_lang$svg$Svg$node('feSpotLight');
var _elm_lang$svg$Svg$altGlyph = _elm_lang$svg$Svg$node('altGlyph');
var _elm_lang$svg$Svg$altGlyphDef = _elm_lang$svg$Svg$node('altGlyphDef');
var _elm_lang$svg$Svg$altGlyphItem = _elm_lang$svg$Svg$node('altGlyphItem');
var _elm_lang$svg$Svg$glyph = _elm_lang$svg$Svg$node('glyph');
var _elm_lang$svg$Svg$glyphRef = _elm_lang$svg$Svg$node('glyphRef');
var _elm_lang$svg$Svg$textPath = _elm_lang$svg$Svg$node('textPath');
var _elm_lang$svg$Svg$text$ = _elm_lang$svg$Svg$node('text');
var _elm_lang$svg$Svg$tref = _elm_lang$svg$Svg$node('tref');
var _elm_lang$svg$Svg$tspan = _elm_lang$svg$Svg$node('tspan');
var _elm_lang$svg$Svg$clipPath = _elm_lang$svg$Svg$node('clipPath');
var _elm_lang$svg$Svg$colorProfile = _elm_lang$svg$Svg$node('colorProfile');
var _elm_lang$svg$Svg$cursor = _elm_lang$svg$Svg$node('cursor');
var _elm_lang$svg$Svg$filter = _elm_lang$svg$Svg$node('filter');
var _elm_lang$svg$Svg$script = _elm_lang$svg$Svg$node('script');
var _elm_lang$svg$Svg$style = _elm_lang$svg$Svg$node('style');
var _elm_lang$svg$Svg$view = _elm_lang$svg$Svg$node('view');

var _elm_lang$svg$Svg_Attributes$writingMode = _elm_lang$virtual_dom$VirtualDom$attribute('writing-mode');
var _elm_lang$svg$Svg_Attributes$wordSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('word-spacing');
var _elm_lang$svg$Svg_Attributes$visibility = _elm_lang$virtual_dom$VirtualDom$attribute('visibility');
var _elm_lang$svg$Svg_Attributes$unicodeBidi = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-bidi');
var _elm_lang$svg$Svg_Attributes$textRendering = _elm_lang$virtual_dom$VirtualDom$attribute('text-rendering');
var _elm_lang$svg$Svg_Attributes$textDecoration = _elm_lang$virtual_dom$VirtualDom$attribute('text-decoration');
var _elm_lang$svg$Svg_Attributes$textAnchor = _elm_lang$virtual_dom$VirtualDom$attribute('text-anchor');
var _elm_lang$svg$Svg_Attributes$stroke = _elm_lang$virtual_dom$VirtualDom$attribute('stroke');
var _elm_lang$svg$Svg_Attributes$strokeWidth = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-width');
var _elm_lang$svg$Svg_Attributes$strokeOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-opacity');
var _elm_lang$svg$Svg_Attributes$strokeMiterlimit = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-miterlimit');
var _elm_lang$svg$Svg_Attributes$strokeLinejoin = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linejoin');
var _elm_lang$svg$Svg_Attributes$strokeLinecap = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linecap');
var _elm_lang$svg$Svg_Attributes$strokeDashoffset = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dashoffset');
var _elm_lang$svg$Svg_Attributes$strokeDasharray = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dasharray');
var _elm_lang$svg$Svg_Attributes$stopOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stop-opacity');
var _elm_lang$svg$Svg_Attributes$stopColor = _elm_lang$virtual_dom$VirtualDom$attribute('stop-color');
var _elm_lang$svg$Svg_Attributes$shapeRendering = _elm_lang$virtual_dom$VirtualDom$attribute('shape-rendering');
var _elm_lang$svg$Svg_Attributes$pointerEvents = _elm_lang$virtual_dom$VirtualDom$attribute('pointer-events');
var _elm_lang$svg$Svg_Attributes$overflow = _elm_lang$virtual_dom$VirtualDom$attribute('overflow');
var _elm_lang$svg$Svg_Attributes$opacity = _elm_lang$virtual_dom$VirtualDom$attribute('opacity');
var _elm_lang$svg$Svg_Attributes$mask = _elm_lang$virtual_dom$VirtualDom$attribute('mask');
var _elm_lang$svg$Svg_Attributes$markerStart = _elm_lang$virtual_dom$VirtualDom$attribute('marker-start');
var _elm_lang$svg$Svg_Attributes$markerMid = _elm_lang$virtual_dom$VirtualDom$attribute('marker-mid');
var _elm_lang$svg$Svg_Attributes$markerEnd = _elm_lang$virtual_dom$VirtualDom$attribute('marker-end');
var _elm_lang$svg$Svg_Attributes$lightingColor = _elm_lang$virtual_dom$VirtualDom$attribute('lighting-color');
var _elm_lang$svg$Svg_Attributes$letterSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('letter-spacing');
var _elm_lang$svg$Svg_Attributes$kerning = _elm_lang$virtual_dom$VirtualDom$attribute('kerning');
var _elm_lang$svg$Svg_Attributes$imageRendering = _elm_lang$virtual_dom$VirtualDom$attribute('image-rendering');
var _elm_lang$svg$Svg_Attributes$glyphOrientationVertical = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-vertical');
var _elm_lang$svg$Svg_Attributes$glyphOrientationHorizontal = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-horizontal');
var _elm_lang$svg$Svg_Attributes$fontWeight = _elm_lang$virtual_dom$VirtualDom$attribute('font-weight');
var _elm_lang$svg$Svg_Attributes$fontVariant = _elm_lang$virtual_dom$VirtualDom$attribute('font-variant');
var _elm_lang$svg$Svg_Attributes$fontStyle = _elm_lang$virtual_dom$VirtualDom$attribute('font-style');
var _elm_lang$svg$Svg_Attributes$fontStretch = _elm_lang$virtual_dom$VirtualDom$attribute('font-stretch');
var _elm_lang$svg$Svg_Attributes$fontSize = _elm_lang$virtual_dom$VirtualDom$attribute('font-size');
var _elm_lang$svg$Svg_Attributes$fontSizeAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('font-size-adjust');
var _elm_lang$svg$Svg_Attributes$fontFamily = _elm_lang$virtual_dom$VirtualDom$attribute('font-family');
var _elm_lang$svg$Svg_Attributes$floodOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('flood-opacity');
var _elm_lang$svg$Svg_Attributes$floodColor = _elm_lang$virtual_dom$VirtualDom$attribute('flood-color');
var _elm_lang$svg$Svg_Attributes$filter = _elm_lang$virtual_dom$VirtualDom$attribute('filter');
var _elm_lang$svg$Svg_Attributes$fill = _elm_lang$virtual_dom$VirtualDom$attribute('fill');
var _elm_lang$svg$Svg_Attributes$fillRule = _elm_lang$virtual_dom$VirtualDom$attribute('fill-rule');
var _elm_lang$svg$Svg_Attributes$fillOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('fill-opacity');
var _elm_lang$svg$Svg_Attributes$enableBackground = _elm_lang$virtual_dom$VirtualDom$attribute('enable-background');
var _elm_lang$svg$Svg_Attributes$dominantBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('dominant-baseline');
var _elm_lang$svg$Svg_Attributes$display = _elm_lang$virtual_dom$VirtualDom$attribute('display');
var _elm_lang$svg$Svg_Attributes$direction = _elm_lang$virtual_dom$VirtualDom$attribute('direction');
var _elm_lang$svg$Svg_Attributes$cursor = _elm_lang$virtual_dom$VirtualDom$attribute('cursor');
var _elm_lang$svg$Svg_Attributes$color = _elm_lang$virtual_dom$VirtualDom$attribute('color');
var _elm_lang$svg$Svg_Attributes$colorRendering = _elm_lang$virtual_dom$VirtualDom$attribute('color-rendering');
var _elm_lang$svg$Svg_Attributes$colorProfile = _elm_lang$virtual_dom$VirtualDom$attribute('color-profile');
var _elm_lang$svg$Svg_Attributes$colorInterpolation = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation');
var _elm_lang$svg$Svg_Attributes$colorInterpolationFilters = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation-filters');
var _elm_lang$svg$Svg_Attributes$clip = _elm_lang$virtual_dom$VirtualDom$attribute('clip');
var _elm_lang$svg$Svg_Attributes$clipRule = _elm_lang$virtual_dom$VirtualDom$attribute('clip-rule');
var _elm_lang$svg$Svg_Attributes$clipPath = _elm_lang$virtual_dom$VirtualDom$attribute('clip-path');
var _elm_lang$svg$Svg_Attributes$baselineShift = _elm_lang$virtual_dom$VirtualDom$attribute('baseline-shift');
var _elm_lang$svg$Svg_Attributes$alignmentBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('alignment-baseline');
var _elm_lang$svg$Svg_Attributes$zoomAndPan = _elm_lang$virtual_dom$VirtualDom$attribute('zoomAndPan');
var _elm_lang$svg$Svg_Attributes$z = _elm_lang$virtual_dom$VirtualDom$attribute('z');
var _elm_lang$svg$Svg_Attributes$yChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('yChannelSelector');
var _elm_lang$svg$Svg_Attributes$y2 = _elm_lang$virtual_dom$VirtualDom$attribute('y2');
var _elm_lang$svg$Svg_Attributes$y1 = _elm_lang$virtual_dom$VirtualDom$attribute('y1');
var _elm_lang$svg$Svg_Attributes$y = _elm_lang$virtual_dom$VirtualDom$attribute('y');
var _elm_lang$svg$Svg_Attributes$xmlSpace = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:space');
var _elm_lang$svg$Svg_Attributes$xmlLang = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:lang');
var _elm_lang$svg$Svg_Attributes$xmlBase = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:base');
var _elm_lang$svg$Svg_Attributes$xlinkType = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:type');
var _elm_lang$svg$Svg_Attributes$xlinkTitle = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:title');
var _elm_lang$svg$Svg_Attributes$xlinkShow = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:show');
var _elm_lang$svg$Svg_Attributes$xlinkRole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:role');
var _elm_lang$svg$Svg_Attributes$xlinkHref = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:href');
var _elm_lang$svg$Svg_Attributes$xlinkArcrole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:arcrole');
var _elm_lang$svg$Svg_Attributes$xlinkActuate = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:actuate');
var _elm_lang$svg$Svg_Attributes$xChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('xChannelSelector');
var _elm_lang$svg$Svg_Attributes$x2 = _elm_lang$virtual_dom$VirtualDom$attribute('x2');
var _elm_lang$svg$Svg_Attributes$x1 = _elm_lang$virtual_dom$VirtualDom$attribute('x1');
var _elm_lang$svg$Svg_Attributes$xHeight = _elm_lang$virtual_dom$VirtualDom$attribute('x-height');
var _elm_lang$svg$Svg_Attributes$x = _elm_lang$virtual_dom$VirtualDom$attribute('x');
var _elm_lang$svg$Svg_Attributes$widths = _elm_lang$virtual_dom$VirtualDom$attribute('widths');
var _elm_lang$svg$Svg_Attributes$width = _elm_lang$virtual_dom$VirtualDom$attribute('width');
var _elm_lang$svg$Svg_Attributes$viewTarget = _elm_lang$virtual_dom$VirtualDom$attribute('viewTarget');
var _elm_lang$svg$Svg_Attributes$viewBox = _elm_lang$virtual_dom$VirtualDom$attribute('viewBox');
var _elm_lang$svg$Svg_Attributes$vertOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-y');
var _elm_lang$svg$Svg_Attributes$vertOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-x');
var _elm_lang$svg$Svg_Attributes$vertAdvY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-adv-y');
var _elm_lang$svg$Svg_Attributes$version = _elm_lang$virtual_dom$VirtualDom$attribute('version');
var _elm_lang$svg$Svg_Attributes$values = _elm_lang$virtual_dom$VirtualDom$attribute('values');
var _elm_lang$svg$Svg_Attributes$vMathematical = _elm_lang$virtual_dom$VirtualDom$attribute('v-mathematical');
var _elm_lang$svg$Svg_Attributes$vIdeographic = _elm_lang$virtual_dom$VirtualDom$attribute('v-ideographic');
var _elm_lang$svg$Svg_Attributes$vHanging = _elm_lang$virtual_dom$VirtualDom$attribute('v-hanging');
var _elm_lang$svg$Svg_Attributes$vAlphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('v-alphabetic');
var _elm_lang$svg$Svg_Attributes$unitsPerEm = _elm_lang$virtual_dom$VirtualDom$attribute('units-per-em');
var _elm_lang$svg$Svg_Attributes$unicodeRange = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-range');
var _elm_lang$svg$Svg_Attributes$unicode = _elm_lang$virtual_dom$VirtualDom$attribute('unicode');
var _elm_lang$svg$Svg_Attributes$underlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('underline-thickness');
var _elm_lang$svg$Svg_Attributes$underlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('underline-position');
var _elm_lang$svg$Svg_Attributes$u2 = _elm_lang$virtual_dom$VirtualDom$attribute('u2');
var _elm_lang$svg$Svg_Attributes$u1 = _elm_lang$virtual_dom$VirtualDom$attribute('u1');
var _elm_lang$svg$Svg_Attributes$type$ = _elm_lang$virtual_dom$VirtualDom$attribute('type');
var _elm_lang$svg$Svg_Attributes$transform = _elm_lang$virtual_dom$VirtualDom$attribute('transform');
var _elm_lang$svg$Svg_Attributes$to = _elm_lang$virtual_dom$VirtualDom$attribute('to');
var _elm_lang$svg$Svg_Attributes$title = _elm_lang$virtual_dom$VirtualDom$attribute('title');
var _elm_lang$svg$Svg_Attributes$textLength = _elm_lang$virtual_dom$VirtualDom$attribute('textLength');
var _elm_lang$svg$Svg_Attributes$targetY = _elm_lang$virtual_dom$VirtualDom$attribute('targetY');
var _elm_lang$svg$Svg_Attributes$targetX = _elm_lang$virtual_dom$VirtualDom$attribute('targetX');
var _elm_lang$svg$Svg_Attributes$target = _elm_lang$virtual_dom$VirtualDom$attribute('target');
var _elm_lang$svg$Svg_Attributes$tableValues = _elm_lang$virtual_dom$VirtualDom$attribute('tableValues');
var _elm_lang$svg$Svg_Attributes$systemLanguage = _elm_lang$virtual_dom$VirtualDom$attribute('systemLanguage');
var _elm_lang$svg$Svg_Attributes$surfaceScale = _elm_lang$virtual_dom$VirtualDom$attribute('surfaceScale');
var _elm_lang$svg$Svg_Attributes$style = _elm_lang$virtual_dom$VirtualDom$attribute('style');
var _elm_lang$svg$Svg_Attributes$string = _elm_lang$virtual_dom$VirtualDom$attribute('string');
var _elm_lang$svg$Svg_Attributes$strikethroughThickness = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-thickness');
var _elm_lang$svg$Svg_Attributes$strikethroughPosition = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-position');
var _elm_lang$svg$Svg_Attributes$stitchTiles = _elm_lang$virtual_dom$VirtualDom$attribute('stitchTiles');
var _elm_lang$svg$Svg_Attributes$stemv = _elm_lang$virtual_dom$VirtualDom$attribute('stemv');
var _elm_lang$svg$Svg_Attributes$stemh = _elm_lang$virtual_dom$VirtualDom$attribute('stemh');
var _elm_lang$svg$Svg_Attributes$stdDeviation = _elm_lang$virtual_dom$VirtualDom$attribute('stdDeviation');
var _elm_lang$svg$Svg_Attributes$startOffset = _elm_lang$virtual_dom$VirtualDom$attribute('startOffset');
var _elm_lang$svg$Svg_Attributes$spreadMethod = _elm_lang$virtual_dom$VirtualDom$attribute('spreadMethod');
var _elm_lang$svg$Svg_Attributes$speed = _elm_lang$virtual_dom$VirtualDom$attribute('speed');
var _elm_lang$svg$Svg_Attributes$specularExponent = _elm_lang$virtual_dom$VirtualDom$attribute('specularExponent');
var _elm_lang$svg$Svg_Attributes$specularConstant = _elm_lang$virtual_dom$VirtualDom$attribute('specularConstant');
var _elm_lang$svg$Svg_Attributes$spacing = _elm_lang$virtual_dom$VirtualDom$attribute('spacing');
var _elm_lang$svg$Svg_Attributes$slope = _elm_lang$virtual_dom$VirtualDom$attribute('slope');
var _elm_lang$svg$Svg_Attributes$seed = _elm_lang$virtual_dom$VirtualDom$attribute('seed');
var _elm_lang$svg$Svg_Attributes$scale = _elm_lang$virtual_dom$VirtualDom$attribute('scale');
var _elm_lang$svg$Svg_Attributes$ry = _elm_lang$virtual_dom$VirtualDom$attribute('ry');
var _elm_lang$svg$Svg_Attributes$rx = _elm_lang$virtual_dom$VirtualDom$attribute('rx');
var _elm_lang$svg$Svg_Attributes$rotate = _elm_lang$virtual_dom$VirtualDom$attribute('rotate');
var _elm_lang$svg$Svg_Attributes$result = _elm_lang$virtual_dom$VirtualDom$attribute('result');
var _elm_lang$svg$Svg_Attributes$restart = _elm_lang$virtual_dom$VirtualDom$attribute('restart');
var _elm_lang$svg$Svg_Attributes$requiredFeatures = _elm_lang$virtual_dom$VirtualDom$attribute('requiredFeatures');
var _elm_lang$svg$Svg_Attributes$requiredExtensions = _elm_lang$virtual_dom$VirtualDom$attribute('requiredExtensions');
var _elm_lang$svg$Svg_Attributes$repeatDur = _elm_lang$virtual_dom$VirtualDom$attribute('repeatDur');
var _elm_lang$svg$Svg_Attributes$repeatCount = _elm_lang$virtual_dom$VirtualDom$attribute('repeatCount');
var _elm_lang$svg$Svg_Attributes$renderingIntent = _elm_lang$virtual_dom$VirtualDom$attribute('rendering-intent');
var _elm_lang$svg$Svg_Attributes$refY = _elm_lang$virtual_dom$VirtualDom$attribute('refY');
var _elm_lang$svg$Svg_Attributes$refX = _elm_lang$virtual_dom$VirtualDom$attribute('refX');
var _elm_lang$svg$Svg_Attributes$radius = _elm_lang$virtual_dom$VirtualDom$attribute('radius');
var _elm_lang$svg$Svg_Attributes$r = _elm_lang$virtual_dom$VirtualDom$attribute('r');
var _elm_lang$svg$Svg_Attributes$primitiveUnits = _elm_lang$virtual_dom$VirtualDom$attribute('primitiveUnits');
var _elm_lang$svg$Svg_Attributes$preserveAspectRatio = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAspectRatio');
var _elm_lang$svg$Svg_Attributes$preserveAlpha = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAlpha');
var _elm_lang$svg$Svg_Attributes$pointsAtZ = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtZ');
var _elm_lang$svg$Svg_Attributes$pointsAtY = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtY');
var _elm_lang$svg$Svg_Attributes$pointsAtX = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtX');
var _elm_lang$svg$Svg_Attributes$points = _elm_lang$virtual_dom$VirtualDom$attribute('points');
var _elm_lang$svg$Svg_Attributes$pointOrder = _elm_lang$virtual_dom$VirtualDom$attribute('point-order');
var _elm_lang$svg$Svg_Attributes$patternUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternUnits');
var _elm_lang$svg$Svg_Attributes$patternTransform = _elm_lang$virtual_dom$VirtualDom$attribute('patternTransform');
var _elm_lang$svg$Svg_Attributes$patternContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternContentUnits');
var _elm_lang$svg$Svg_Attributes$pathLength = _elm_lang$virtual_dom$VirtualDom$attribute('pathLength');
var _elm_lang$svg$Svg_Attributes$path = _elm_lang$virtual_dom$VirtualDom$attribute('path');
var _elm_lang$svg$Svg_Attributes$panose1 = _elm_lang$virtual_dom$VirtualDom$attribute('panose-1');
var _elm_lang$svg$Svg_Attributes$overlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('overline-thickness');
var _elm_lang$svg$Svg_Attributes$overlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('overline-position');
var _elm_lang$svg$Svg_Attributes$origin = _elm_lang$virtual_dom$VirtualDom$attribute('origin');
var _elm_lang$svg$Svg_Attributes$orientation = _elm_lang$virtual_dom$VirtualDom$attribute('orientation');
var _elm_lang$svg$Svg_Attributes$orient = _elm_lang$virtual_dom$VirtualDom$attribute('orient');
var _elm_lang$svg$Svg_Attributes$order = _elm_lang$virtual_dom$VirtualDom$attribute('order');
var _elm_lang$svg$Svg_Attributes$operator = _elm_lang$virtual_dom$VirtualDom$attribute('operator');
var _elm_lang$svg$Svg_Attributes$offset = _elm_lang$virtual_dom$VirtualDom$attribute('offset');
var _elm_lang$svg$Svg_Attributes$numOctaves = _elm_lang$virtual_dom$VirtualDom$attribute('numOctaves');
var _elm_lang$svg$Svg_Attributes$name = _elm_lang$virtual_dom$VirtualDom$attribute('name');
var _elm_lang$svg$Svg_Attributes$mode = _elm_lang$virtual_dom$VirtualDom$attribute('mode');
var _elm_lang$svg$Svg_Attributes$min = _elm_lang$virtual_dom$VirtualDom$attribute('min');
var _elm_lang$svg$Svg_Attributes$method = _elm_lang$virtual_dom$VirtualDom$attribute('method');
var _elm_lang$svg$Svg_Attributes$media = _elm_lang$virtual_dom$VirtualDom$attribute('media');
var _elm_lang$svg$Svg_Attributes$max = _elm_lang$virtual_dom$VirtualDom$attribute('max');
var _elm_lang$svg$Svg_Attributes$mathematical = _elm_lang$virtual_dom$VirtualDom$attribute('mathematical');
var _elm_lang$svg$Svg_Attributes$maskUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskUnits');
var _elm_lang$svg$Svg_Attributes$maskContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskContentUnits');
var _elm_lang$svg$Svg_Attributes$markerWidth = _elm_lang$virtual_dom$VirtualDom$attribute('markerWidth');
var _elm_lang$svg$Svg_Attributes$markerUnits = _elm_lang$virtual_dom$VirtualDom$attribute('markerUnits');
var _elm_lang$svg$Svg_Attributes$markerHeight = _elm_lang$virtual_dom$VirtualDom$attribute('markerHeight');
var _elm_lang$svg$Svg_Attributes$local = _elm_lang$virtual_dom$VirtualDom$attribute('local');
var _elm_lang$svg$Svg_Attributes$limitingConeAngle = _elm_lang$virtual_dom$VirtualDom$attribute('limitingConeAngle');
var _elm_lang$svg$Svg_Attributes$lengthAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('lengthAdjust');
var _elm_lang$svg$Svg_Attributes$lang = _elm_lang$virtual_dom$VirtualDom$attribute('lang');
var _elm_lang$svg$Svg_Attributes$keyTimes = _elm_lang$virtual_dom$VirtualDom$attribute('keyTimes');
var _elm_lang$svg$Svg_Attributes$keySplines = _elm_lang$virtual_dom$VirtualDom$attribute('keySplines');
var _elm_lang$svg$Svg_Attributes$keyPoints = _elm_lang$virtual_dom$VirtualDom$attribute('keyPoints');
var _elm_lang$svg$Svg_Attributes$kernelUnitLength = _elm_lang$virtual_dom$VirtualDom$attribute('kernelUnitLength');
var _elm_lang$svg$Svg_Attributes$kernelMatrix = _elm_lang$virtual_dom$VirtualDom$attribute('kernelMatrix');
var _elm_lang$svg$Svg_Attributes$k4 = _elm_lang$virtual_dom$VirtualDom$attribute('k4');
var _elm_lang$svg$Svg_Attributes$k3 = _elm_lang$virtual_dom$VirtualDom$attribute('k3');
var _elm_lang$svg$Svg_Attributes$k2 = _elm_lang$virtual_dom$VirtualDom$attribute('k2');
var _elm_lang$svg$Svg_Attributes$k1 = _elm_lang$virtual_dom$VirtualDom$attribute('k1');
var _elm_lang$svg$Svg_Attributes$k = _elm_lang$virtual_dom$VirtualDom$attribute('k');
var _elm_lang$svg$Svg_Attributes$intercept = _elm_lang$virtual_dom$VirtualDom$attribute('intercept');
var _elm_lang$svg$Svg_Attributes$in2 = _elm_lang$virtual_dom$VirtualDom$attribute('in2');
var _elm_lang$svg$Svg_Attributes$in$ = _elm_lang$virtual_dom$VirtualDom$attribute('in');
var _elm_lang$svg$Svg_Attributes$ideographic = _elm_lang$virtual_dom$VirtualDom$attribute('ideographic');
var _elm_lang$svg$Svg_Attributes$id = _elm_lang$virtual_dom$VirtualDom$attribute('id');
var _elm_lang$svg$Svg_Attributes$horizOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-y');
var _elm_lang$svg$Svg_Attributes$horizOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-x');
var _elm_lang$svg$Svg_Attributes$horizAdvX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-adv-x');
var _elm_lang$svg$Svg_Attributes$height = _elm_lang$virtual_dom$VirtualDom$attribute('height');
var _elm_lang$svg$Svg_Attributes$hanging = _elm_lang$virtual_dom$VirtualDom$attribute('hanging');
var _elm_lang$svg$Svg_Attributes$gradientUnits = _elm_lang$virtual_dom$VirtualDom$attribute('gradientUnits');
var _elm_lang$svg$Svg_Attributes$gradientTransform = _elm_lang$virtual_dom$VirtualDom$attribute('gradientTransform');
var _elm_lang$svg$Svg_Attributes$glyphRef = _elm_lang$virtual_dom$VirtualDom$attribute('glyphRef');
var _elm_lang$svg$Svg_Attributes$glyphName = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-name');
var _elm_lang$svg$Svg_Attributes$g2 = _elm_lang$virtual_dom$VirtualDom$attribute('g2');
var _elm_lang$svg$Svg_Attributes$g1 = _elm_lang$virtual_dom$VirtualDom$attribute('g1');
var _elm_lang$svg$Svg_Attributes$fy = _elm_lang$virtual_dom$VirtualDom$attribute('fy');
var _elm_lang$svg$Svg_Attributes$fx = _elm_lang$virtual_dom$VirtualDom$attribute('fx');
var _elm_lang$svg$Svg_Attributes$from = _elm_lang$virtual_dom$VirtualDom$attribute('from');
var _elm_lang$svg$Svg_Attributes$format = _elm_lang$virtual_dom$VirtualDom$attribute('format');
var _elm_lang$svg$Svg_Attributes$filterUnits = _elm_lang$virtual_dom$VirtualDom$attribute('filterUnits');
var _elm_lang$svg$Svg_Attributes$filterRes = _elm_lang$virtual_dom$VirtualDom$attribute('filterRes');
var _elm_lang$svg$Svg_Attributes$externalResourcesRequired = _elm_lang$virtual_dom$VirtualDom$attribute('externalResourcesRequired');
var _elm_lang$svg$Svg_Attributes$exponent = _elm_lang$virtual_dom$VirtualDom$attribute('exponent');
var _elm_lang$svg$Svg_Attributes$end = _elm_lang$virtual_dom$VirtualDom$attribute('end');
var _elm_lang$svg$Svg_Attributes$elevation = _elm_lang$virtual_dom$VirtualDom$attribute('elevation');
var _elm_lang$svg$Svg_Attributes$edgeMode = _elm_lang$virtual_dom$VirtualDom$attribute('edgeMode');
var _elm_lang$svg$Svg_Attributes$dy = _elm_lang$virtual_dom$VirtualDom$attribute('dy');
var _elm_lang$svg$Svg_Attributes$dx = _elm_lang$virtual_dom$VirtualDom$attribute('dx');
var _elm_lang$svg$Svg_Attributes$dur = _elm_lang$virtual_dom$VirtualDom$attribute('dur');
var _elm_lang$svg$Svg_Attributes$divisor = _elm_lang$virtual_dom$VirtualDom$attribute('divisor');
var _elm_lang$svg$Svg_Attributes$diffuseConstant = _elm_lang$virtual_dom$VirtualDom$attribute('diffuseConstant');
var _elm_lang$svg$Svg_Attributes$descent = _elm_lang$virtual_dom$VirtualDom$attribute('descent');
var _elm_lang$svg$Svg_Attributes$decelerate = _elm_lang$virtual_dom$VirtualDom$attribute('decelerate');
var _elm_lang$svg$Svg_Attributes$d = _elm_lang$virtual_dom$VirtualDom$attribute('d');
var _elm_lang$svg$Svg_Attributes$cy = _elm_lang$virtual_dom$VirtualDom$attribute('cy');
var _elm_lang$svg$Svg_Attributes$cx = _elm_lang$virtual_dom$VirtualDom$attribute('cx');
var _elm_lang$svg$Svg_Attributes$contentStyleType = _elm_lang$virtual_dom$VirtualDom$attribute('contentStyleType');
var _elm_lang$svg$Svg_Attributes$contentScriptType = _elm_lang$virtual_dom$VirtualDom$attribute('contentScriptType');
var _elm_lang$svg$Svg_Attributes$clipPathUnits = _elm_lang$virtual_dom$VirtualDom$attribute('clipPathUnits');
var _elm_lang$svg$Svg_Attributes$class = _elm_lang$virtual_dom$VirtualDom$attribute('class');
var _elm_lang$svg$Svg_Attributes$capHeight = _elm_lang$virtual_dom$VirtualDom$attribute('cap-height');
var _elm_lang$svg$Svg_Attributes$calcMode = _elm_lang$virtual_dom$VirtualDom$attribute('calcMode');
var _elm_lang$svg$Svg_Attributes$by = _elm_lang$virtual_dom$VirtualDom$attribute('by');
var _elm_lang$svg$Svg_Attributes$bias = _elm_lang$virtual_dom$VirtualDom$attribute('bias');
var _elm_lang$svg$Svg_Attributes$begin = _elm_lang$virtual_dom$VirtualDom$attribute('begin');
var _elm_lang$svg$Svg_Attributes$bbox = _elm_lang$virtual_dom$VirtualDom$attribute('bbox');
var _elm_lang$svg$Svg_Attributes$baseProfile = _elm_lang$virtual_dom$VirtualDom$attribute('baseProfile');
var _elm_lang$svg$Svg_Attributes$baseFrequency = _elm_lang$virtual_dom$VirtualDom$attribute('baseFrequency');
var _elm_lang$svg$Svg_Attributes$azimuth = _elm_lang$virtual_dom$VirtualDom$attribute('azimuth');
var _elm_lang$svg$Svg_Attributes$autoReverse = _elm_lang$virtual_dom$VirtualDom$attribute('autoReverse');
var _elm_lang$svg$Svg_Attributes$attributeType = _elm_lang$virtual_dom$VirtualDom$attribute('attributeType');
var _elm_lang$svg$Svg_Attributes$attributeName = _elm_lang$virtual_dom$VirtualDom$attribute('attributeName');
var _elm_lang$svg$Svg_Attributes$ascent = _elm_lang$virtual_dom$VirtualDom$attribute('ascent');
var _elm_lang$svg$Svg_Attributes$arabicForm = _elm_lang$virtual_dom$VirtualDom$attribute('arabic-form');
var _elm_lang$svg$Svg_Attributes$amplitude = _elm_lang$virtual_dom$VirtualDom$attribute('amplitude');
var _elm_lang$svg$Svg_Attributes$allowReorder = _elm_lang$virtual_dom$VirtualDom$attribute('allowReorder');
var _elm_lang$svg$Svg_Attributes$alphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('alphabetic');
var _elm_lang$svg$Svg_Attributes$additive = _elm_lang$virtual_dom$VirtualDom$attribute('additive');
var _elm_lang$svg$Svg_Attributes$accumulate = _elm_lang$virtual_dom$VirtualDom$attribute('accumulate');
var _elm_lang$svg$Svg_Attributes$accelerate = _elm_lang$virtual_dom$VirtualDom$attribute('accelerate');
var _elm_lang$svg$Svg_Attributes$accentHeight = _elm_lang$virtual_dom$VirtualDom$attribute('accent-height');

var _elm_lang$svg$Svg_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
var _elm_lang$svg$Svg_Events$simpleOn = F2(
	function (name, msg) {
		return A2(
			_elm_lang$svg$Svg_Events$on,
			name,
			_elm_lang$core$Json_Decode$succeed(msg));
	});
var _elm_lang$svg$Svg_Events$onBegin = _elm_lang$svg$Svg_Events$simpleOn('begin');
var _elm_lang$svg$Svg_Events$onEnd = _elm_lang$svg$Svg_Events$simpleOn('end');
var _elm_lang$svg$Svg_Events$onRepeat = _elm_lang$svg$Svg_Events$simpleOn('repeat');
var _elm_lang$svg$Svg_Events$onAbort = _elm_lang$svg$Svg_Events$simpleOn('abort');
var _elm_lang$svg$Svg_Events$onError = _elm_lang$svg$Svg_Events$simpleOn('error');
var _elm_lang$svg$Svg_Events$onResize = _elm_lang$svg$Svg_Events$simpleOn('resize');
var _elm_lang$svg$Svg_Events$onScroll = _elm_lang$svg$Svg_Events$simpleOn('scroll');
var _elm_lang$svg$Svg_Events$onLoad = _elm_lang$svg$Svg_Events$simpleOn('load');
var _elm_lang$svg$Svg_Events$onUnload = _elm_lang$svg$Svg_Events$simpleOn('unload');
var _elm_lang$svg$Svg_Events$onZoom = _elm_lang$svg$Svg_Events$simpleOn('zoom');
var _elm_lang$svg$Svg_Events$onActivate = _elm_lang$svg$Svg_Events$simpleOn('activate');
var _elm_lang$svg$Svg_Events$onClick = _elm_lang$svg$Svg_Events$simpleOn('click');
var _elm_lang$svg$Svg_Events$onFocusIn = _elm_lang$svg$Svg_Events$simpleOn('focusin');
var _elm_lang$svg$Svg_Events$onFocusOut = _elm_lang$svg$Svg_Events$simpleOn('focusout');
var _elm_lang$svg$Svg_Events$onMouseDown = _elm_lang$svg$Svg_Events$simpleOn('mousedown');
var _elm_lang$svg$Svg_Events$onMouseMove = _elm_lang$svg$Svg_Events$simpleOn('mousemove');
var _elm_lang$svg$Svg_Events$onMouseOut = _elm_lang$svg$Svg_Events$simpleOn('mouseout');
var _elm_lang$svg$Svg_Events$onMouseOver = _elm_lang$svg$Svg_Events$simpleOn('mouseover');
var _elm_lang$svg$Svg_Events$onMouseUp = _elm_lang$svg$Svg_Events$simpleOn('mouseup');

var _mgold$elm_random_pcg$Random_Pcg$toJson = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$core$Json_Encode$list(
		_elm_lang$core$Native_List.fromArray(
			[
				_elm_lang$core$Json_Encode$int(_p1._0),
				_elm_lang$core$Json_Encode$int(_p1._1)
			]));
};
var _mgold$elm_random_pcg$Random_Pcg$mul32 = F2(
	function (a, b) {
		var bl = A2(_elm_lang$core$Bitwise$and, b, 65535);
		var bh = A2(
			_elm_lang$core$Bitwise$and,
			A2(_elm_lang$core$Bitwise$shiftRightLogical, b, 16),
			65535);
		var al = A2(_elm_lang$core$Bitwise$and, a, 65535);
		var ah = A2(
			_elm_lang$core$Bitwise$and,
			A2(_elm_lang$core$Bitwise$shiftRightLogical, a, 16),
			65535);
		return A2(
			_elm_lang$core$Bitwise$or,
			0,
			(al * bl) + A2(
				_elm_lang$core$Bitwise$shiftRightLogical,
				A2(_elm_lang$core$Bitwise$shiftLeft, (ah * bl) + (al * bh), 16),
				0));
	});
var _mgold$elm_random_pcg$Random_Pcg$listHelp = F4(
	function (list, n, generate, seed) {
		listHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 1) < 0) {
				return {ctor: '_Tuple2', _0: list, _1: seed};
			} else {
				var _p2 = generate(seed);
				var value = _p2._0;
				var newSeed = _p2._1;
				var _v1 = A2(_elm_lang$core$List_ops['::'], value, list),
					_v2 = n - 1,
					_v3 = generate,
					_v4 = newSeed;
				list = _v1;
				n = _v2;
				generate = _v3;
				seed = _v4;
				continue listHelp;
			}
		}
	});
var _mgold$elm_random_pcg$Random_Pcg$minInt = -2147483648;
var _mgold$elm_random_pcg$Random_Pcg$maxInt = 2147483647;
var _mgold$elm_random_pcg$Random_Pcg$bit27 = 1.34217728e8;
var _mgold$elm_random_pcg$Random_Pcg$bit53 = 9.007199254740992e15;
var _mgold$elm_random_pcg$Random_Pcg$peel = function (_p3) {
	var _p4 = _p3;
	var _p5 = _p4._0;
	var word = A2(
		_elm_lang$core$Bitwise$xor,
		A2(
			_elm_lang$core$Bitwise$shiftRightLogical,
			_p5,
			A2(_elm_lang$core$Bitwise$shiftRightLogical, _p5, 28) + 4),
		_p5) * 277803737;
	return A2(
		_elm_lang$core$Bitwise$shiftRightLogical,
		A2(
			_elm_lang$core$Bitwise$xor,
			A2(_elm_lang$core$Bitwise$shiftRightLogical, word, 22),
			word),
		0);
};
var _mgold$elm_random_pcg$Random_Pcg$never = function (a) {
	never:
	while (true) {
		var _v6 = a;
		a = _v6;
		continue never;
	}
};
var _mgold$elm_random_pcg$Random_Pcg$step = F2(
	function (_p6, seed) {
		var _p7 = _p6;
		return _p7._0(seed);
	});
var _mgold$elm_random_pcg$Random_Pcg$retry = F3(
	function (generator, predicate, seed) {
		retry:
		while (true) {
			var _p8 = A2(_mgold$elm_random_pcg$Random_Pcg$step, generator, seed);
			var candidate = _p8._0;
			var newSeed = _p8._1;
			if (predicate(candidate)) {
				return {ctor: '_Tuple2', _0: candidate, _1: newSeed};
			} else {
				var _v8 = generator,
					_v9 = predicate,
					_v10 = newSeed;
				generator = _v8;
				predicate = _v9;
				seed = _v10;
				continue retry;
			}
		}
	});
var _mgold$elm_random_pcg$Random_Pcg$Generator = function (a) {
	return {ctor: 'Generator', _0: a};
};
var _mgold$elm_random_pcg$Random_Pcg$list = F2(
	function (n, _p9) {
		var _p10 = _p9;
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed) {
				return A4(
					_mgold$elm_random_pcg$Random_Pcg$listHelp,
					_elm_lang$core$Native_List.fromArray(
						[]),
					n,
					_p10._0,
					seed);
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$constant = function (value) {
	return _mgold$elm_random_pcg$Random_Pcg$Generator(
		function (seed) {
			return {ctor: '_Tuple2', _0: value, _1: seed};
		});
};
var _mgold$elm_random_pcg$Random_Pcg$map = F2(
	function (func, _p11) {
		var _p12 = _p11;
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed0) {
				var _p13 = _p12._0(seed0);
				var a = _p13._0;
				var seed1 = _p13._1;
				return {
					ctor: '_Tuple2',
					_0: func(a),
					_1: seed1
				};
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$map2 = F3(
	function (func, _p15, _p14) {
		var _p16 = _p15;
		var _p17 = _p14;
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed0) {
				var _p18 = _p16._0(seed0);
				var a = _p18._0;
				var seed1 = _p18._1;
				var _p19 = _p17._0(seed1);
				var b = _p19._0;
				var seed2 = _p19._1;
				return {
					ctor: '_Tuple2',
					_0: A2(func, a, b),
					_1: seed2
				};
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$pair = F2(
	function (genA, genB) {
		return A3(
			_mgold$elm_random_pcg$Random_Pcg$map2,
			F2(
				function (v0, v1) {
					return {ctor: '_Tuple2', _0: v0, _1: v1};
				}),
			genA,
			genB);
	});
var _mgold$elm_random_pcg$Random_Pcg$andMap = _mgold$elm_random_pcg$Random_Pcg$map2(
	F2(
		function (x, y) {
			return x(y);
		}));
var _mgold$elm_random_pcg$Random_Pcg$map3 = F4(
	function (func, _p22, _p21, _p20) {
		var _p23 = _p22;
		var _p24 = _p21;
		var _p25 = _p20;
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed0) {
				var _p26 = _p23._0(seed0);
				var a = _p26._0;
				var seed1 = _p26._1;
				var _p27 = _p24._0(seed1);
				var b = _p27._0;
				var seed2 = _p27._1;
				var _p28 = _p25._0(seed2);
				var c = _p28._0;
				var seed3 = _p28._1;
				return {
					ctor: '_Tuple2',
					_0: A3(func, a, b, c),
					_1: seed3
				};
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$map4 = F5(
	function (func, _p32, _p31, _p30, _p29) {
		var _p33 = _p32;
		var _p34 = _p31;
		var _p35 = _p30;
		var _p36 = _p29;
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed0) {
				var _p37 = _p33._0(seed0);
				var a = _p37._0;
				var seed1 = _p37._1;
				var _p38 = _p34._0(seed1);
				var b = _p38._0;
				var seed2 = _p38._1;
				var _p39 = _p35._0(seed2);
				var c = _p39._0;
				var seed3 = _p39._1;
				var _p40 = _p36._0(seed3);
				var d = _p40._0;
				var seed4 = _p40._1;
				return {
					ctor: '_Tuple2',
					_0: A4(func, a, b, c, d),
					_1: seed4
				};
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$map5 = F6(
	function (func, _p45, _p44, _p43, _p42, _p41) {
		var _p46 = _p45;
		var _p47 = _p44;
		var _p48 = _p43;
		var _p49 = _p42;
		var _p50 = _p41;
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed0) {
				var _p51 = _p46._0(seed0);
				var a = _p51._0;
				var seed1 = _p51._1;
				var _p52 = _p47._0(seed1);
				var b = _p52._0;
				var seed2 = _p52._1;
				var _p53 = _p48._0(seed2);
				var c = _p53._0;
				var seed3 = _p53._1;
				var _p54 = _p49._0(seed3);
				var d = _p54._0;
				var seed4 = _p54._1;
				var _p55 = _p50._0(seed4);
				var e = _p55._0;
				var seed5 = _p55._1;
				return {
					ctor: '_Tuple2',
					_0: A5(func, a, b, c, d, e),
					_1: seed5
				};
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$andThen = F2(
	function (_p56, callback) {
		var _p57 = _p56;
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed) {
				var _p58 = _p57._0(seed);
				var result = _p58._0;
				var newSeed = _p58._1;
				var _p59 = callback(result);
				var generateB = _p59._0;
				return generateB(newSeed);
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$maybe = F2(
	function (genBool, genA) {
		return A2(
			_mgold$elm_random_pcg$Random_Pcg$andThen,
			genBool,
			function (b) {
				return b ? A2(_mgold$elm_random_pcg$Random_Pcg$map, _elm_lang$core$Maybe$Just, genA) : _mgold$elm_random_pcg$Random_Pcg$constant(_elm_lang$core$Maybe$Nothing);
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$filter = F2(
	function (predicate, generator) {
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			A2(_mgold$elm_random_pcg$Random_Pcg$retry, generator, predicate));
	});
var _mgold$elm_random_pcg$Random_Pcg$Seed = F2(
	function (a, b) {
		return {ctor: 'Seed', _0: a, _1: b};
	});
var _mgold$elm_random_pcg$Random_Pcg$next = function (_p60) {
	var _p61 = _p60;
	var _p62 = _p61._1;
	return A2(
		_mgold$elm_random_pcg$Random_Pcg$Seed,
		A2(_elm_lang$core$Bitwise$shiftRightLogical, (_p61._0 * 1664525) + _p62, 0),
		_p62);
};
var _mgold$elm_random_pcg$Random_Pcg$initialSeed = function (x) {
	var _p63 = _mgold$elm_random_pcg$Random_Pcg$next(
		A2(_mgold$elm_random_pcg$Random_Pcg$Seed, 0, 1013904223));
	var state1 = _p63._0;
	var incr = _p63._1;
	var state2 = A2(_elm_lang$core$Bitwise$shiftRightLogical, state1 + x, 0);
	return _mgold$elm_random_pcg$Random_Pcg$next(
		A2(_mgold$elm_random_pcg$Random_Pcg$Seed, state2, incr));
};
var _mgold$elm_random_pcg$Random_Pcg$generate = F2(
	function (toMsg, generator) {
		return A3(
			_elm_lang$core$Task$perform,
			_mgold$elm_random_pcg$Random_Pcg$never,
			toMsg,
			A2(
				_elm_lang$core$Task$map,
				function (_p64) {
					return _elm_lang$core$Basics$fst(
						A2(
							_mgold$elm_random_pcg$Random_Pcg$step,
							generator,
							_mgold$elm_random_pcg$Random_Pcg$initialSeed(
								_elm_lang$core$Basics$round(_p64))));
				},
				_elm_lang$core$Time$now));
	});
var _mgold$elm_random_pcg$Random_Pcg$int = F2(
	function (a, b) {
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed0) {
				var _p65 = (_elm_lang$core$Native_Utils.cmp(a, b) < 0) ? {ctor: '_Tuple2', _0: a, _1: b} : {ctor: '_Tuple2', _0: b, _1: a};
				var lo = _p65._0;
				var hi = _p65._1;
				var range = (hi - lo) + 1;
				if (_elm_lang$core$Native_Utils.eq(
					A2(_elm_lang$core$Bitwise$and, range, range - 1),
					0)) {
					return {
						ctor: '_Tuple2',
						_0: A2(
							_elm_lang$core$Bitwise$shiftRightLogical,
							A2(
								_elm_lang$core$Bitwise$and,
								_mgold$elm_random_pcg$Random_Pcg$peel(seed0),
								range - 1),
							0) + lo,
						_1: _mgold$elm_random_pcg$Random_Pcg$next(seed0)
					};
				} else {
					var threshhold = A2(
						_elm_lang$core$Bitwise$shiftRightLogical,
						A2(
							_elm_lang$core$Basics$rem,
							A2(_elm_lang$core$Bitwise$shiftRightLogical, 0 - range, 0),
							range),
						0);
					var accountForBias = function (seed) {
						accountForBias:
						while (true) {
							var seedN = _mgold$elm_random_pcg$Random_Pcg$next(seed);
							var x = _mgold$elm_random_pcg$Random_Pcg$peel(seed);
							if (_elm_lang$core$Native_Utils.cmp(x, threshhold) < 0) {
								var _v29 = seedN;
								seed = _v29;
								continue accountForBias;
							} else {
								return {
									ctor: '_Tuple2',
									_0: A2(_elm_lang$core$Basics$rem, x, range) + lo,
									_1: seedN
								};
							}
						}
					};
					return accountForBias(seed0);
				}
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$bool = A2(
	_mgold$elm_random_pcg$Random_Pcg$map,
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.eq(x, y);
		})(1),
	A2(_mgold$elm_random_pcg$Random_Pcg$int, 0, 1));
var _mgold$elm_random_pcg$Random_Pcg$choice = F2(
	function (x, y) {
		return A2(
			_mgold$elm_random_pcg$Random_Pcg$map,
			function (b) {
				return b ? x : y;
			},
			_mgold$elm_random_pcg$Random_Pcg$bool);
	});
var _mgold$elm_random_pcg$Random_Pcg$oneIn = function (n) {
	return A2(
		_mgold$elm_random_pcg$Random_Pcg$map,
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(1),
		A2(_mgold$elm_random_pcg$Random_Pcg$int, 1, n));
};
var _mgold$elm_random_pcg$Random_Pcg$sample = function () {
	var find = F2(
		function (k, ys) {
			find:
			while (true) {
				var _p66 = ys;
				if (_p66.ctor === '[]') {
					return _elm_lang$core$Maybe$Nothing;
				} else {
					if (_elm_lang$core$Native_Utils.eq(k, 0)) {
						return _elm_lang$core$Maybe$Just(_p66._0);
					} else {
						var _v31 = k - 1,
							_v32 = _p66._1;
						k = _v31;
						ys = _v32;
						continue find;
					}
				}
			}
		});
	return function (xs) {
		return A2(
			_mgold$elm_random_pcg$Random_Pcg$map,
			function (i) {
				return A2(find, i, xs);
			},
			A2(
				_mgold$elm_random_pcg$Random_Pcg$int,
				0,
				_elm_lang$core$List$length(xs) - 1));
	};
}();
var _mgold$elm_random_pcg$Random_Pcg$float = F2(
	function (min, max) {
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed0) {
				var range = _elm_lang$core$Basics$abs(max - min);
				var n0 = _mgold$elm_random_pcg$Random_Pcg$peel(seed0);
				var hi = _elm_lang$core$Basics$toFloat(
					A2(_elm_lang$core$Bitwise$and, n0, 67108863)) * 1.0;
				var seed1 = _mgold$elm_random_pcg$Random_Pcg$next(seed0);
				var n1 = _mgold$elm_random_pcg$Random_Pcg$peel(seed1);
				var lo = _elm_lang$core$Basics$toFloat(
					A2(_elm_lang$core$Bitwise$and, n1, 134217727)) * 1.0;
				var val = ((hi * _mgold$elm_random_pcg$Random_Pcg$bit27) + lo) / _mgold$elm_random_pcg$Random_Pcg$bit53;
				var scaled = (val * range) + min;
				return {
					ctor: '_Tuple2',
					_0: scaled,
					_1: _mgold$elm_random_pcg$Random_Pcg$next(seed1)
				};
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$frequency = function (pairs) {
	var pick = F2(
		function (choices, n) {
			pick:
			while (true) {
				var _p67 = choices;
				if ((_p67.ctor === '::') && (_p67._0.ctor === '_Tuple2')) {
					var _p68 = _p67._0._0;
					if (_elm_lang$core$Native_Utils.cmp(n, _p68) < 1) {
						return _p67._0._1;
					} else {
						var _v34 = _p67._1,
							_v35 = n - _p68;
						choices = _v34;
						n = _v35;
						continue pick;
					}
				} else {
					return _elm_lang$core$Native_Utils.crashCase(
						'Random.Pcg',
						{
							start: {line: 688, column: 13},
							end: {line: 696, column: 77}
						},
						_p67)('Empty list passed to Random.Pcg.frequency!');
				}
			}
		});
	var total = _elm_lang$core$List$sum(
		A2(
			_elm_lang$core$List$map,
			function (_p70) {
				return _elm_lang$core$Basics$abs(
					_elm_lang$core$Basics$fst(_p70));
			},
			pairs));
	return A2(
		_mgold$elm_random_pcg$Random_Pcg$andThen,
		A2(_mgold$elm_random_pcg$Random_Pcg$float, 0, total),
		pick(pairs));
};
var _mgold$elm_random_pcg$Random_Pcg$choices = function (gens) {
	return _mgold$elm_random_pcg$Random_Pcg$frequency(
		A2(
			_elm_lang$core$List$map,
			function (g) {
				return {ctor: '_Tuple2', _0: 1, _1: g};
			},
			gens));
};
var _mgold$elm_random_pcg$Random_Pcg$independentSeed = _mgold$elm_random_pcg$Random_Pcg$Generator(
	function (seed0) {
		var gen = A2(_mgold$elm_random_pcg$Random_Pcg$int, 0, 4294967295);
		var _p71 = A2(
			_mgold$elm_random_pcg$Random_Pcg$step,
			A4(
				_mgold$elm_random_pcg$Random_Pcg$map3,
				F3(
					function (v0, v1, v2) {
						return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
					}),
				gen,
				gen,
				gen),
			seed0);
		var state = _p71._0._0;
		var b = _p71._0._1;
		var c = _p71._0._2;
		var seed1 = _p71._1;
		var incr = A2(
			_elm_lang$core$Bitwise$or,
			A2(_elm_lang$core$Bitwise$xor, b, c),
			1);
		return {
			ctor: '_Tuple2',
			_0: seed1,
			_1: _mgold$elm_random_pcg$Random_Pcg$next(
				A2(_mgold$elm_random_pcg$Random_Pcg$Seed, state, incr))
		};
	});
var _mgold$elm_random_pcg$Random_Pcg$fastForward = F2(
	function (delta0, _p72) {
		var _p73 = _p72;
		var _p76 = _p73._1;
		var helper = F6(
			function (accMult, accPlus, curMult, curPlus, delta, repeat) {
				helper:
				while (true) {
					var newDelta = A2(_elm_lang$core$Bitwise$shiftRightLogical, delta, 1);
					var curMult$ = A2(_mgold$elm_random_pcg$Random_Pcg$mul32, curMult, curMult);
					var curPlus$ = A2(_mgold$elm_random_pcg$Random_Pcg$mul32, curMult + 1, curPlus);
					var _p74 = _elm_lang$core$Native_Utils.eq(
						A2(_elm_lang$core$Bitwise$and, delta, 1),
						1) ? {
						ctor: '_Tuple2',
						_0: A2(_mgold$elm_random_pcg$Random_Pcg$mul32, accMult, curMult),
						_1: A2(
							_elm_lang$core$Bitwise$shiftRightLogical,
							A2(_mgold$elm_random_pcg$Random_Pcg$mul32, accPlus, curMult) + curPlus,
							0)
					} : {ctor: '_Tuple2', _0: accMult, _1: accPlus};
					var accMult$ = _p74._0;
					var accPlus$ = _p74._1;
					if (_elm_lang$core$Native_Utils.eq(newDelta, 0)) {
						if ((_elm_lang$core$Native_Utils.cmp(delta0, 0) < 0) && repeat) {
							var _v37 = accMult$,
								_v38 = accPlus$,
								_v39 = curMult$,
								_v40 = curPlus$,
								_v41 = -1,
								_v42 = false;
							accMult = _v37;
							accPlus = _v38;
							curMult = _v39;
							curPlus = _v40;
							delta = _v41;
							repeat = _v42;
							continue helper;
						} else {
							return {ctor: '_Tuple2', _0: accMult$, _1: accPlus$};
						}
					} else {
						var _v43 = accMult$,
							_v44 = accPlus$,
							_v45 = curMult$,
							_v46 = curPlus$,
							_v47 = newDelta,
							_v48 = repeat;
						accMult = _v43;
						accPlus = _v44;
						curMult = _v45;
						curPlus = _v46;
						delta = _v47;
						repeat = _v48;
						continue helper;
					}
				}
			});
		var _p75 = A6(helper, 1, 0, 1664525, _p76, delta0, true);
		var accMultFinal = _p75._0;
		var accPlusFinal = _p75._1;
		return A2(
			_mgold$elm_random_pcg$Random_Pcg$Seed,
			A2(
				_elm_lang$core$Bitwise$shiftRightLogical,
				A2(_mgold$elm_random_pcg$Random_Pcg$mul32, accMultFinal, _p73._0) + accPlusFinal,
				0),
			_p76);
	});
var _mgold$elm_random_pcg$Random_Pcg$fromJson = _elm_lang$core$Json_Decode$oneOf(
	_elm_lang$core$Native_List.fromArray(
		[
			A3(_elm_lang$core$Json_Decode$tuple2, _mgold$elm_random_pcg$Random_Pcg$Seed, _elm_lang$core$Json_Decode$int, _elm_lang$core$Json_Decode$int),
			A2(_elm_lang$core$Json_Decode$map, _mgold$elm_random_pcg$Random_Pcg$initialSeed, _elm_lang$core$Json_Decode$int)
		]));

var _user$project$Extras$orElseLazy = F2(
	function (fma, mb) {
		var _p0 = mb;
		if (_p0.ctor === 'Nothing') {
			return fma(
				{ctor: '_Tuple0'});
		} else {
			return mb;
		}
	});
var _user$project$Extras$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			var _p1 = list;
			if (_p1.ctor === '[]') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p2 = _p1._0;
				if (predicate(_p2)) {
					return _elm_lang$core$Maybe$Just(_p2);
				} else {
					var _v2 = predicate,
						_v3 = _p1._1;
					predicate = _v2;
					list = _v3;
					continue find;
				}
			}
		}
	});

var _user$project$Model$stashSet = F3(
	function (size, amount, stash) {
		var _p0 = size;
		switch (_p0.ctor) {
			case 'Queen':
				return _elm_lang$core$Native_Utils.update(
					stash,
					{queen: amount});
			case 'Drone':
				return _elm_lang$core$Native_Utils.update(
					stash,
					{drone: amount});
			default:
				return _elm_lang$core$Native_Utils.update(
					stash,
					{pawn: amount});
		}
	});
var _user$project$Model$stashGet = F2(
	function (size, stash) {
		var _p1 = size;
		switch (_p1.ctor) {
			case 'Queen':
				return stash.queen;
			case 'Drone':
				return stash.drone;
			default:
				return stash.pawn;
		}
	});
var _user$project$Model$stashIsEmpty = function (_p2) {
	var _p3 = _p2;
	return (_elm_lang$core$Native_Utils.cmp(_p3.queen, 0) < 1) && ((_elm_lang$core$Native_Utils.cmp(_p3.drone, 0) < 1) && (_elm_lang$core$Native_Utils.cmp(_p3.pawn, 0) < 1));
};
var _user$project$Model$maxStashAmount = 5;
var _user$project$Model$threeByThreeAny = F2(
	function (predicate, record) {
		return predicate(record.zeroZero) || (predicate(record.zeroOne) || (predicate(record.zeroTwo) || (predicate(record.oneZero) || (predicate(record.oneOne) || (predicate(record.oneTwo) || (predicate(record.twoOne) || (predicate(record.twoZero) || predicate(record.twoTwo))))))));
	});
var _user$project$Model$Model = F6(
	function (a, b, c, d, e, f) {
		return {mdl: a, selected: b, board: c, stash: d, player: e, outcome: f};
	});
var _user$project$Model$Stash = F3(
	function (a, b, c) {
		return {queen: a, drone: b, pawn: c};
	});
var _user$project$Model$defaultStash = A3(_user$project$Model$Stash, _user$project$Model$maxStashAmount, _user$project$Model$maxStashAmount, _user$project$Model$maxStashAmount);
var _user$project$Model$ThreeByThreeRecord = F9(
	function (a, b, c, d, e, f, g, h, i) {
		return {zeroZero: a, zeroOne: b, zeroTwo: c, oneZero: d, oneOne: e, oneTwo: f, twoOne: g, twoZero: h, twoTwo: i};
	});
var _user$project$Model$CPU = {ctor: 'CPU'};
var _user$project$Model$User = {ctor: 'User'};
var _user$project$Model$nextPlayer = function (player) {
	var _p4 = player;
	if (_p4.ctor === 'User') {
		return _user$project$Model$CPU;
	} else {
		return _user$project$Model$User;
	}
};
var _user$project$Model$TBD = {ctor: 'TBD'};
var _user$project$Model$Tie = {ctor: 'Tie'};
var _user$project$Model$CPUWinByExhaustion = {ctor: 'CPUWinByExhaustion'};
var _user$project$Model$CPUWin = {ctor: 'CPUWin'};
var _user$project$Model$UserWinByExhaustion = {ctor: 'UserWinByExhaustion'};
var _user$project$Model$UserWin = {ctor: 'UserWin'};
var _user$project$Model$subOutcomeToOutcome = F2(
	function (player, subOutcome) {
		var _p5 = subOutcome;
		if (_p5.ctor === 'Undetermined') {
			return _user$project$Model$TBD;
		} else {
			var _p6 = player;
			if (_p6.ctor === 'User') {
				return _user$project$Model$UserWin;
			} else {
				return _user$project$Model$CPUWin;
			}
		}
	});
var _user$project$Model$Undetermined = {ctor: 'Undetermined'};
var _user$project$Model$Win = {ctor: 'Win'};
var _user$project$Model$checkSizes = F3(
	function (size1, size2, size3) {
		return (_elm_lang$core$Native_Utils.eq(size1, size2) && _elm_lang$core$Native_Utils.eq(size2, size3)) ? _user$project$Model$Win : _user$project$Model$Undetermined;
	});
var _user$project$Model$andCheckSize = F4(
	function (size1, size2, size3, subOutcome) {
		var _p7 = subOutcome;
		if (_p7.ctor === 'Undetermined') {
			return A3(_user$project$Model$checkSizes, size1, size2, size3);
		} else {
			return _p7;
		}
	});
var _user$project$Model$Pawn = {ctor: 'Pawn'};
var _user$project$Model$Drone = {ctor: 'Drone'};
var _user$project$Model$Queen = {ctor: 'Queen'};
var _user$project$Model$getAvailableSizes = function (stash) {
	return A2(
		_elm_lang$core$List$filter,
		function (size) {
			return _elm_lang$core$Native_Utils.cmp(
				A2(_user$project$Model$stashGet, size, stash),
				0) > 0;
		},
		_elm_lang$core$Native_List.fromArray(
			[_user$project$Model$Queen, _user$project$Model$Drone, _user$project$Model$Pawn]));
};
var _user$project$Model$sizeFits = F2(
	function (size, stack) {
		var _p8 = stack;
		switch (_p8.ctor) {
			case 'EmptyStack':
				return true;
			case 'Single':
				return !_elm_lang$core$Native_Utils.eq(size, _p8._0);
			case 'PartialTree':
				return _elm_lang$core$Native_Utils.eq(size, _user$project$Model$Pawn);
			case 'PartialNest':
				return _elm_lang$core$Native_Utils.eq(size, _user$project$Model$Queen);
			case 'FullTree':
				return false;
			case 'DroneTree':
				return false;
			case 'NoDroneTree':
				return false;
			case 'DroneNest':
				return false;
			case 'FullNest':
				return false;
			default:
				return false;
		}
	});
var _user$project$Model$canPlace = F2(
	function (size, model) {
		if (_elm_lang$core$Native_Utils.cmp(
			A2(_user$project$Model$stashGet, size, model.stash),
			0) < 1) {
			return false;
		} else {
			var _p9 = model.board;
			if (_p9.ctor === 'ThreeByThree') {
				return A2(
					_user$project$Model$threeByThreeAny,
					_user$project$Model$sizeFits(size),
					_p9._0);
			} else {
				return true;
			}
		}
	});
var _user$project$Model$canPlaceQueen = _user$project$Model$canPlace(_user$project$Model$Queen);
var _user$project$Model$canPlaceDrone = _user$project$Model$canPlace(_user$project$Model$Drone);
var _user$project$Model$canPlacePawn = _user$project$Model$canPlace(_user$project$Model$Pawn);
var _user$project$Model$noLegalMoves = function (model) {
	var hasLegalMoves = _user$project$Model$canPlaceQueen(model) || (_user$project$Model$canPlaceDrone(model) || _user$project$Model$canPlacePawn(model));
	return _elm_lang$core$Basics$not(hasLegalMoves);
};
var _user$project$Model$NoDroneNest = {ctor: 'NoDroneNest'};
var _user$project$Model$DroneNest = {ctor: 'DroneNest'};
var _user$project$Model$PartialNest = {ctor: 'PartialNest'};
var _user$project$Model$FullNest = {ctor: 'FullNest'};
var _user$project$Model$NoDroneTree = {ctor: 'NoDroneTree'};
var _user$project$Model$DroneTree = {ctor: 'DroneTree'};
var _user$project$Model$PartialTree = {ctor: 'PartialTree'};
var _user$project$Model$FullTree = {ctor: 'FullTree'};
var _user$project$Model$Single = function (a) {
	return {ctor: 'Single', _0: a};
};
var _user$project$Model$checkLine = F3(
	function (stack1, stack2, stack3) {
		checkLine:
		while (true) {
			var _p10 = {ctor: '_Tuple3', _0: stack1, _1: stack2, _2: stack3};
			_v9_34:
			do {
				_v9_33:
				do {
					_v9_31:
					do {
						_v9_30:
						do {
							_v9_29:
							do {
								_v9_27:
								do {
									_v9_26:
									do {
										_v9_25:
										do {
											_v9_23:
											do {
												_v9_22:
												do {
													_v9_21:
													do {
														_v9_19:
														do {
															_v9_18:
															do {
																_v9_17:
																do {
																	_v9_15:
																	do {
																		_v9_14:
																		do {
																			_v9_13:
																			do {
																				_v9_11:
																				do {
																					_v9_10:
																					do {
																						_v9_9:
																						do {
																							_v9_7:
																							do {
																								_v9_6:
																								do {
																									_v9_5:
																									do {
																										_v9_2:
																										do {
																											_v9_1:
																											do {
																												switch (_p10._0.ctor) {
																													case 'EmptyStack':
																														return _user$project$Model$Undetermined;
																													case 'Single':
																														switch (_p10._1.ctor) {
																															case 'EmptyStack':
																																break _v9_1;
																															case 'Single':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'Single':
																																		return A3(_user$project$Model$checkSizes, _p10._0._0, _p10._1._0, _p10._2._0);
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_23;
																																	case 'PartialNest':
																																		break _v9_27;
																																	case 'DroneNest':
																																		break _v9_31;
																																	default:
																																		var _v10 = stack1,
																																			_v11 = stack2,
																																			_v12 = _user$project$Model$Single(_user$project$Model$Queen);
																																		stack1 = _v10;
																																		stack2 = _v11;
																																		stack3 = _v12;
																																		continue checkLine;
																																}
																															case 'FullTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_6;
																																	case 'PartialTree':
																																		break _v9_6;
																																	case 'DroneTree':
																																		break _v9_6;
																																	case 'NoDroneTree':
																																		break _v9_6;
																																	case 'FullNest':
																																		break _v9_6;
																																	case 'PartialNest':
																																		break _v9_6;
																																	case 'DroneNest':
																																		break _v9_6;
																																	case 'NoDroneNest':
																																		break _v9_6;
																																	default:
																																		break _v9_6;
																																}
																															case 'PartialTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_10;
																																	case 'DroneTree':
																																		break _v9_10;
																																	case 'NoDroneTree':
																																		break _v9_10;
																																	case 'FullNest':
																																		break _v9_10;
																																	case 'PartialNest':
																																		break _v9_10;
																																	case 'DroneNest':
																																		break _v9_10;
																																	case 'NoDroneNest':
																																		break _v9_10;
																																	default:
																																		break _v9_10;
																																}
																															case 'DroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_14;
																																	case 'NoDroneTree':
																																		break _v9_14;
																																	case 'FullNest':
																																		break _v9_14;
																																	case 'PartialNest':
																																		break _v9_14;
																																	case 'DroneNest':
																																		break _v9_14;
																																	case 'NoDroneNest':
																																		break _v9_14;
																																	default:
																																		break _v9_14;
																																}
																															case 'NoDroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_18;
																																	case 'FullNest':
																																		break _v9_18;
																																	case 'PartialNest':
																																		break _v9_18;
																																	case 'DroneNest':
																																		break _v9_18;
																																	case 'NoDroneNest':
																																		break _v9_18;
																																	default:
																																		break _v9_18;
																																}
																															case 'FullNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_22;
																																	case 'PartialNest':
																																		break _v9_22;
																																	case 'DroneNest':
																																		break _v9_22;
																																	case 'NoDroneNest':
																																		break _v9_22;
																																	default:
																																		break _v9_22;
																																}
																															case 'PartialNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_23;
																																	case 'PartialNest':
																																		break _v9_26;
																																	case 'DroneNest':
																																		break _v9_26;
																																	case 'NoDroneNest':
																																		break _v9_26;
																																	default:
																																		break _v9_26;
																																}
																															case 'DroneNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_23;
																																	case 'PartialNest':
																																		break _v9_27;
																																	case 'DroneNest':
																																		break _v9_30;
																																	case 'NoDroneNest':
																																		break _v9_30;
																																	default:
																																		break _v9_30;
																																}
																															default:
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_23;
																																	case 'PartialNest':
																																		break _v9_27;
																																	case 'DroneNest':
																																		break _v9_31;
																																	case 'NoDroneNest':
																																		break _v9_34;
																																	default:
																																		break _v9_34;
																																}
																														}
																													case 'FullTree':
																														switch (_p10._1.ctor) {
																															case 'EmptyStack':
																																break _v9_1;
																															case 'FullTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		return _user$project$Model$Win;
																																	case 'PartialTree':
																																		break _v9_5;
																																	case 'DroneTree':
																																		break _v9_5;
																																	case 'NoDroneTree':
																																		break _v9_5;
																																	case 'FullNest':
																																		break _v9_5;
																																	case 'PartialNest':
																																		break _v9_5;
																																	case 'DroneNest':
																																		break _v9_5;
																																	case 'NoDroneNest':
																																		break _v9_5;
																																	default:
																																		break _v9_5;
																																}
																															case 'PartialTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_5;
																																	case 'PartialTree':
																																		break _v9_5;
																																	case 'DroneTree':
																																		break _v9_5;
																																	case 'NoDroneTree':
																																		break _v9_5;
																																	case 'FullNest':
																																		break _v9_5;
																																	case 'PartialNest':
																																		break _v9_5;
																																	case 'DroneNest':
																																		break _v9_5;
																																	case 'NoDroneNest':
																																		break _v9_5;
																																	default:
																																		break _v9_5;
																																}
																															case 'DroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_5;
																																	case 'PartialTree':
																																		break _v9_5;
																																	case 'DroneTree':
																																		break _v9_5;
																																	case 'NoDroneTree':
																																		break _v9_5;
																																	case 'FullNest':
																																		break _v9_5;
																																	case 'PartialNest':
																																		break _v9_5;
																																	case 'DroneNest':
																																		break _v9_5;
																																	case 'NoDroneNest':
																																		break _v9_5;
																																	default:
																																		break _v9_5;
																																}
																															case 'NoDroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_5;
																																	case 'PartialTree':
																																		break _v9_5;
																																	case 'DroneTree':
																																		break _v9_5;
																																	case 'NoDroneTree':
																																		break _v9_5;
																																	case 'FullNest':
																																		break _v9_5;
																																	case 'PartialNest':
																																		break _v9_5;
																																	case 'DroneNest':
																																		break _v9_5;
																																	case 'NoDroneNest':
																																		break _v9_5;
																																	default:
																																		break _v9_5;
																																}
																															case 'FullNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_5;
																																	case 'PartialTree':
																																		break _v9_5;
																																	case 'DroneTree':
																																		break _v9_5;
																																	case 'NoDroneTree':
																																		break _v9_5;
																																	case 'FullNest':
																																		break _v9_5;
																																	case 'PartialNest':
																																		break _v9_5;
																																	case 'DroneNest':
																																		break _v9_5;
																																	case 'NoDroneNest':
																																		break _v9_5;
																																	default:
																																		break _v9_5;
																																}
																															case 'PartialNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_5;
																																	case 'PartialTree':
																																		break _v9_5;
																																	case 'DroneTree':
																																		break _v9_5;
																																	case 'NoDroneTree':
																																		break _v9_5;
																																	case 'FullNest':
																																		break _v9_5;
																																	case 'PartialNest':
																																		break _v9_5;
																																	case 'DroneNest':
																																		break _v9_5;
																																	case 'NoDroneNest':
																																		break _v9_5;
																																	default:
																																		break _v9_5;
																																}
																															case 'DroneNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_5;
																																	case 'PartialTree':
																																		break _v9_5;
																																	case 'DroneTree':
																																		break _v9_5;
																																	case 'NoDroneTree':
																																		break _v9_5;
																																	case 'FullNest':
																																		break _v9_5;
																																	case 'PartialNest':
																																		break _v9_5;
																																	case 'DroneNest':
																																		break _v9_5;
																																	case 'NoDroneNest':
																																		break _v9_5;
																																	default:
																																		break _v9_5;
																																}
																															case 'NoDroneNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_5;
																																	case 'PartialTree':
																																		break _v9_5;
																																	case 'DroneTree':
																																		break _v9_5;
																																	case 'NoDroneTree':
																																		break _v9_5;
																																	case 'FullNest':
																																		break _v9_5;
																																	case 'PartialNest':
																																		break _v9_5;
																																	case 'DroneNest':
																																		break _v9_5;
																																	case 'NoDroneNest':
																																		break _v9_5;
																																	default:
																																		break _v9_5;
																																}
																															default:
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_5;
																																	case 'PartialTree':
																																		break _v9_5;
																																	case 'DroneTree':
																																		break _v9_5;
																																	case 'NoDroneTree':
																																		break _v9_5;
																																	case 'FullNest':
																																		break _v9_5;
																																	case 'PartialNest':
																																		break _v9_5;
																																	case 'DroneNest':
																																		break _v9_5;
																																	case 'NoDroneNest':
																																		break _v9_5;
																																	default:
																																		break _v9_5;
																																}
																														}
																													case 'PartialTree':
																														switch (_p10._1.ctor) {
																															case 'EmptyStack':
																																break _v9_1;
																															case 'FullTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_6;
																																	case 'PartialTree':
																																		break _v9_6;
																																	case 'DroneTree':
																																		break _v9_6;
																																	case 'NoDroneTree':
																																		break _v9_6;
																																	case 'FullNest':
																																		break _v9_6;
																																	case 'PartialNest':
																																		break _v9_6;
																																	case 'DroneNest':
																																		break _v9_6;
																																	case 'NoDroneNest':
																																		break _v9_6;
																																	default:
																																		break _v9_6;
																																}
																															case 'PartialTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		return _user$project$Model$Win;
																																	case 'DroneTree':
																																		break _v9_9;
																																	case 'NoDroneTree':
																																		break _v9_9;
																																	case 'FullNest':
																																		break _v9_9;
																																	case 'PartialNest':
																																		break _v9_9;
																																	case 'DroneNest':
																																		break _v9_9;
																																	case 'NoDroneNest':
																																		break _v9_9;
																																	default:
																																		break _v9_9;
																																}
																															case 'DroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_9;
																																	case 'DroneTree':
																																		break _v9_9;
																																	case 'NoDroneTree':
																																		break _v9_9;
																																	case 'FullNest':
																																		break _v9_9;
																																	case 'PartialNest':
																																		break _v9_9;
																																	case 'DroneNest':
																																		break _v9_9;
																																	case 'NoDroneNest':
																																		break _v9_9;
																																	default:
																																		break _v9_9;
																																}
																															case 'NoDroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_9;
																																	case 'DroneTree':
																																		break _v9_9;
																																	case 'NoDroneTree':
																																		break _v9_9;
																																	case 'FullNest':
																																		break _v9_9;
																																	case 'PartialNest':
																																		break _v9_9;
																																	case 'DroneNest':
																																		break _v9_9;
																																	case 'NoDroneNest':
																																		break _v9_9;
																																	default:
																																		break _v9_9;
																																}
																															case 'FullNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_9;
																																	case 'DroneTree':
																																		break _v9_9;
																																	case 'NoDroneTree':
																																		break _v9_9;
																																	case 'FullNest':
																																		break _v9_9;
																																	case 'PartialNest':
																																		break _v9_9;
																																	case 'DroneNest':
																																		break _v9_9;
																																	case 'NoDroneNest':
																																		break _v9_9;
																																	default:
																																		break _v9_9;
																																}
																															case 'PartialNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_9;
																																	case 'DroneTree':
																																		break _v9_9;
																																	case 'NoDroneTree':
																																		break _v9_9;
																																	case 'FullNest':
																																		break _v9_9;
																																	case 'PartialNest':
																																		break _v9_9;
																																	case 'DroneNest':
																																		break _v9_9;
																																	case 'NoDroneNest':
																																		break _v9_9;
																																	default:
																																		break _v9_9;
																																}
																															case 'DroneNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_9;
																																	case 'DroneTree':
																																		break _v9_9;
																																	case 'NoDroneTree':
																																		break _v9_9;
																																	case 'FullNest':
																																		break _v9_9;
																																	case 'PartialNest':
																																		break _v9_9;
																																	case 'DroneNest':
																																		break _v9_9;
																																	case 'NoDroneNest':
																																		break _v9_9;
																																	default:
																																		break _v9_9;
																																}
																															case 'NoDroneNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_9;
																																	case 'DroneTree':
																																		break _v9_9;
																																	case 'NoDroneTree':
																																		break _v9_9;
																																	case 'FullNest':
																																		break _v9_9;
																																	case 'PartialNest':
																																		break _v9_9;
																																	case 'DroneNest':
																																		break _v9_9;
																																	case 'NoDroneNest':
																																		break _v9_9;
																																	default:
																																		break _v9_9;
																																}
																															default:
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_9;
																																	case 'DroneTree':
																																		break _v9_9;
																																	case 'NoDroneTree':
																																		break _v9_9;
																																	case 'FullNest':
																																		break _v9_9;
																																	case 'PartialNest':
																																		break _v9_9;
																																	case 'DroneNest':
																																		break _v9_9;
																																	case 'NoDroneNest':
																																		break _v9_9;
																																	default:
																																		break _v9_9;
																																}
																														}
																													case 'DroneTree':
																														switch (_p10._1.ctor) {
																															case 'EmptyStack':
																																break _v9_1;
																															case 'FullTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_6;
																																	case 'PartialTree':
																																		break _v9_6;
																																	case 'DroneTree':
																																		break _v9_6;
																																	case 'NoDroneTree':
																																		break _v9_6;
																																	case 'FullNest':
																																		break _v9_6;
																																	case 'PartialNest':
																																		break _v9_6;
																																	case 'DroneNest':
																																		break _v9_6;
																																	case 'NoDroneNest':
																																		break _v9_6;
																																	default:
																																		break _v9_6;
																																}
																															case 'PartialTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_10;
																																	case 'DroneTree':
																																		break _v9_10;
																																	case 'NoDroneTree':
																																		break _v9_10;
																																	case 'FullNest':
																																		break _v9_10;
																																	case 'PartialNest':
																																		break _v9_10;
																																	case 'DroneNest':
																																		break _v9_10;
																																	case 'NoDroneNest':
																																		break _v9_10;
																																	default:
																																		break _v9_10;
																																}
																															case 'DroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		return _user$project$Model$Win;
																																	case 'NoDroneTree':
																																		break _v9_13;
																																	case 'FullNest':
																																		break _v9_13;
																																	case 'PartialNest':
																																		break _v9_13;
																																	case 'DroneNest':
																																		break _v9_13;
																																	case 'NoDroneNest':
																																		break _v9_13;
																																	default:
																																		break _v9_13;
																																}
																															case 'NoDroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_13;
																																	case 'NoDroneTree':
																																		break _v9_13;
																																	case 'FullNest':
																																		break _v9_13;
																																	case 'PartialNest':
																																		break _v9_13;
																																	case 'DroneNest':
																																		break _v9_13;
																																	case 'NoDroneNest':
																																		break _v9_13;
																																	default:
																																		break _v9_13;
																																}
																															case 'FullNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_13;
																																	case 'NoDroneTree':
																																		break _v9_13;
																																	case 'FullNest':
																																		break _v9_13;
																																	case 'PartialNest':
																																		break _v9_13;
																																	case 'DroneNest':
																																		break _v9_13;
																																	case 'NoDroneNest':
																																		break _v9_13;
																																	default:
																																		break _v9_13;
																																}
																															case 'PartialNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_13;
																																	case 'NoDroneTree':
																																		break _v9_13;
																																	case 'FullNest':
																																		break _v9_13;
																																	case 'PartialNest':
																																		break _v9_13;
																																	case 'DroneNest':
																																		break _v9_13;
																																	case 'NoDroneNest':
																																		break _v9_13;
																																	default:
																																		break _v9_13;
																																}
																															case 'DroneNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_13;
																																	case 'NoDroneTree':
																																		break _v9_13;
																																	case 'FullNest':
																																		break _v9_13;
																																	case 'PartialNest':
																																		break _v9_13;
																																	case 'DroneNest':
																																		break _v9_13;
																																	case 'NoDroneNest':
																																		break _v9_13;
																																	default:
																																		break _v9_13;
																																}
																															case 'NoDroneNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_13;
																																	case 'NoDroneTree':
																																		break _v9_13;
																																	case 'FullNest':
																																		break _v9_13;
																																	case 'PartialNest':
																																		break _v9_13;
																																	case 'DroneNest':
																																		break _v9_13;
																																	case 'NoDroneNest':
																																		break _v9_13;
																																	default:
																																		break _v9_13;
																																}
																															default:
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_13;
																																	case 'NoDroneTree':
																																		break _v9_13;
																																	case 'FullNest':
																																		break _v9_13;
																																	case 'PartialNest':
																																		break _v9_13;
																																	case 'DroneNest':
																																		break _v9_13;
																																	case 'NoDroneNest':
																																		break _v9_13;
																																	default:
																																		break _v9_13;
																																}
																														}
																													case 'NoDroneTree':
																														switch (_p10._1.ctor) {
																															case 'EmptyStack':
																																break _v9_1;
																															case 'FullTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_6;
																																	case 'PartialTree':
																																		break _v9_6;
																																	case 'DroneTree':
																																		break _v9_6;
																																	case 'NoDroneTree':
																																		break _v9_6;
																																	case 'FullNest':
																																		break _v9_6;
																																	case 'PartialNest':
																																		break _v9_6;
																																	case 'DroneNest':
																																		break _v9_6;
																																	case 'NoDroneNest':
																																		break _v9_6;
																																	default:
																																		break _v9_6;
																																}
																															case 'PartialTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_10;
																																	case 'DroneTree':
																																		break _v9_10;
																																	case 'NoDroneTree':
																																		break _v9_10;
																																	case 'FullNest':
																																		break _v9_10;
																																	case 'PartialNest':
																																		break _v9_10;
																																	case 'DroneNest':
																																		break _v9_10;
																																	case 'NoDroneNest':
																																		break _v9_10;
																																	default:
																																		break _v9_10;
																																}
																															case 'DroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_14;
																																	case 'NoDroneTree':
																																		break _v9_14;
																																	case 'FullNest':
																																		break _v9_14;
																																	case 'PartialNest':
																																		break _v9_14;
																																	case 'DroneNest':
																																		break _v9_14;
																																	case 'NoDroneNest':
																																		break _v9_14;
																																	default:
																																		break _v9_14;
																																}
																															case 'NoDroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		return _user$project$Model$Win;
																																	case 'FullNest':
																																		break _v9_17;
																																	case 'PartialNest':
																																		break _v9_17;
																																	case 'DroneNest':
																																		break _v9_17;
																																	case 'NoDroneNest':
																																		break _v9_17;
																																	default:
																																		break _v9_17;
																																}
																															case 'FullNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_17;
																																	case 'FullNest':
																																		break _v9_17;
																																	case 'PartialNest':
																																		break _v9_17;
																																	case 'DroneNest':
																																		break _v9_17;
																																	case 'NoDroneNest':
																																		break _v9_17;
																																	default:
																																		break _v9_17;
																																}
																															case 'PartialNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_17;
																																	case 'FullNest':
																																		break _v9_17;
																																	case 'PartialNest':
																																		break _v9_17;
																																	case 'DroneNest':
																																		break _v9_17;
																																	case 'NoDroneNest':
																																		break _v9_17;
																																	default:
																																		break _v9_17;
																																}
																															case 'DroneNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_17;
																																	case 'FullNest':
																																		break _v9_17;
																																	case 'PartialNest':
																																		break _v9_17;
																																	case 'DroneNest':
																																		break _v9_17;
																																	case 'NoDroneNest':
																																		break _v9_17;
																																	default:
																																		break _v9_17;
																																}
																															case 'NoDroneNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_17;
																																	case 'FullNest':
																																		break _v9_17;
																																	case 'PartialNest':
																																		break _v9_17;
																																	case 'DroneNest':
																																		break _v9_17;
																																	case 'NoDroneNest':
																																		break _v9_17;
																																	default:
																																		break _v9_17;
																																}
																															default:
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_17;
																																	case 'FullNest':
																																		break _v9_17;
																																	case 'PartialNest':
																																		break _v9_17;
																																	case 'DroneNest':
																																		break _v9_17;
																																	case 'NoDroneNest':
																																		break _v9_17;
																																	default:
																																		break _v9_17;
																																}
																														}
																													case 'FullNest':
																														switch (_p10._1.ctor) {
																															case 'EmptyStack':
																																break _v9_1;
																															case 'FullTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_6;
																																	case 'PartialTree':
																																		break _v9_6;
																																	case 'DroneTree':
																																		break _v9_6;
																																	case 'NoDroneTree':
																																		break _v9_6;
																																	case 'FullNest':
																																		break _v9_6;
																																	case 'PartialNest':
																																		break _v9_6;
																																	case 'DroneNest':
																																		break _v9_6;
																																	case 'NoDroneNest':
																																		break _v9_6;
																																	default:
																																		break _v9_6;
																																}
																															case 'PartialTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_10;
																																	case 'DroneTree':
																																		break _v9_10;
																																	case 'NoDroneTree':
																																		break _v9_10;
																																	case 'FullNest':
																																		break _v9_10;
																																	case 'PartialNest':
																																		break _v9_10;
																																	case 'DroneNest':
																																		break _v9_10;
																																	case 'NoDroneNest':
																																		break _v9_10;
																																	default:
																																		break _v9_10;
																																}
																															case 'DroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_14;
																																	case 'NoDroneTree':
																																		break _v9_14;
																																	case 'FullNest':
																																		break _v9_14;
																																	case 'PartialNest':
																																		break _v9_14;
																																	case 'DroneNest':
																																		break _v9_14;
																																	case 'NoDroneNest':
																																		break _v9_14;
																																	default:
																																		break _v9_14;
																																}
																															case 'NoDroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_18;
																																	case 'FullNest':
																																		break _v9_18;
																																	case 'PartialNest':
																																		break _v9_18;
																																	case 'DroneNest':
																																		break _v9_18;
																																	case 'NoDroneNest':
																																		break _v9_18;
																																	default:
																																		break _v9_18;
																																}
																															case 'FullNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		return _user$project$Model$Win;
																																	case 'PartialNest':
																																		break _v9_21;
																																	case 'DroneNest':
																																		break _v9_21;
																																	case 'NoDroneNest':
																																		break _v9_21;
																																	default:
																																		break _v9_21;
																																}
																															case 'PartialNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_21;
																																	case 'PartialNest':
																																		break _v9_21;
																																	case 'DroneNest':
																																		break _v9_21;
																																	case 'NoDroneNest':
																																		break _v9_21;
																																	default:
																																		break _v9_21;
																																}
																															case 'DroneNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_21;
																																	case 'PartialNest':
																																		break _v9_21;
																																	case 'DroneNest':
																																		break _v9_21;
																																	case 'NoDroneNest':
																																		break _v9_21;
																																	default:
																																		break _v9_21;
																																}
																															case 'NoDroneNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_21;
																																	case 'PartialNest':
																																		break _v9_21;
																																	case 'DroneNest':
																																		break _v9_21;
																																	case 'NoDroneNest':
																																		break _v9_21;
																																	default:
																																		break _v9_21;
																																}
																															default:
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_21;
																																	case 'PartialNest':
																																		break _v9_21;
																																	case 'DroneNest':
																																		break _v9_21;
																																	case 'NoDroneNest':
																																		break _v9_21;
																																	default:
																																		break _v9_21;
																																}
																														}
																													case 'PartialNest':
																														switch (_p10._1.ctor) {
																															case 'EmptyStack':
																																break _v9_1;
																															case 'FullTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_6;
																																	case 'PartialTree':
																																		break _v9_6;
																																	case 'DroneTree':
																																		break _v9_6;
																																	case 'NoDroneTree':
																																		break _v9_6;
																																	case 'FullNest':
																																		break _v9_6;
																																	case 'PartialNest':
																																		break _v9_6;
																																	case 'DroneNest':
																																		break _v9_6;
																																	case 'NoDroneNest':
																																		break _v9_6;
																																	default:
																																		break _v9_6;
																																}
																															case 'PartialTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_10;
																																	case 'DroneTree':
																																		break _v9_10;
																																	case 'NoDroneTree':
																																		break _v9_10;
																																	case 'FullNest':
																																		break _v9_10;
																																	case 'PartialNest':
																																		break _v9_10;
																																	case 'DroneNest':
																																		break _v9_10;
																																	case 'NoDroneNest':
																																		break _v9_10;
																																	default:
																																		break _v9_10;
																																}
																															case 'DroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_14;
																																	case 'NoDroneTree':
																																		break _v9_14;
																																	case 'FullNest':
																																		break _v9_14;
																																	case 'PartialNest':
																																		break _v9_14;
																																	case 'DroneNest':
																																		break _v9_14;
																																	case 'NoDroneNest':
																																		break _v9_14;
																																	default:
																																		break _v9_14;
																																}
																															case 'NoDroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_18;
																																	case 'FullNest':
																																		break _v9_18;
																																	case 'PartialNest':
																																		break _v9_18;
																																	case 'DroneNest':
																																		break _v9_18;
																																	case 'NoDroneNest':
																																		break _v9_18;
																																	default:
																																		break _v9_18;
																																}
																															case 'FullNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_22;
																																	case 'PartialNest':
																																		break _v9_22;
																																	case 'DroneNest':
																																		break _v9_22;
																																	case 'NoDroneNest':
																																		break _v9_22;
																																	default:
																																		break _v9_22;
																																}
																															case 'PartialNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_23;
																																	case 'PartialNest':
																																		return _user$project$Model$Win;
																																	case 'DroneNest':
																																		break _v9_25;
																																	case 'NoDroneNest':
																																		break _v9_25;
																																	default:
																																		break _v9_25;
																																}
																															case 'DroneNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_23;
																																	case 'PartialNest':
																																		break _v9_25;
																																	case 'DroneNest':
																																		break _v9_25;
																																	case 'NoDroneNest':
																																		break _v9_25;
																																	default:
																																		break _v9_25;
																																}
																															case 'NoDroneNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_23;
																																	case 'PartialNest':
																																		break _v9_25;
																																	case 'DroneNest':
																																		break _v9_25;
																																	case 'NoDroneNest':
																																		break _v9_25;
																																	default:
																																		break _v9_25;
																																}
																															default:
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_23;
																																	case 'PartialNest':
																																		break _v9_25;
																																	case 'DroneNest':
																																		break _v9_25;
																																	case 'NoDroneNest':
																																		break _v9_25;
																																	default:
																																		break _v9_25;
																																}
																														}
																													case 'DroneNest':
																														switch (_p10._1.ctor) {
																															case 'EmptyStack':
																																break _v9_1;
																															case 'FullTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_6;
																																	case 'PartialTree':
																																		break _v9_6;
																																	case 'DroneTree':
																																		break _v9_6;
																																	case 'NoDroneTree':
																																		break _v9_6;
																																	case 'FullNest':
																																		break _v9_6;
																																	case 'PartialNest':
																																		break _v9_6;
																																	case 'DroneNest':
																																		break _v9_6;
																																	case 'NoDroneNest':
																																		break _v9_6;
																																	default:
																																		break _v9_6;
																																}
																															case 'PartialTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_10;
																																	case 'DroneTree':
																																		break _v9_10;
																																	case 'NoDroneTree':
																																		break _v9_10;
																																	case 'FullNest':
																																		break _v9_10;
																																	case 'PartialNest':
																																		break _v9_10;
																																	case 'DroneNest':
																																		break _v9_10;
																																	case 'NoDroneNest':
																																		break _v9_10;
																																	default:
																																		break _v9_10;
																																}
																															case 'DroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_14;
																																	case 'NoDroneTree':
																																		break _v9_14;
																																	case 'FullNest':
																																		break _v9_14;
																																	case 'PartialNest':
																																		break _v9_14;
																																	case 'DroneNest':
																																		break _v9_14;
																																	case 'NoDroneNest':
																																		break _v9_14;
																																	default:
																																		break _v9_14;
																																}
																															case 'NoDroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_18;
																																	case 'FullNest':
																																		break _v9_18;
																																	case 'PartialNest':
																																		break _v9_18;
																																	case 'DroneNest':
																																		break _v9_18;
																																	case 'NoDroneNest':
																																		break _v9_18;
																																	default:
																																		break _v9_18;
																																}
																															case 'FullNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_22;
																																	case 'PartialNest':
																																		break _v9_22;
																																	case 'DroneNest':
																																		break _v9_22;
																																	case 'NoDroneNest':
																																		break _v9_22;
																																	default:
																																		break _v9_22;
																																}
																															case 'PartialNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_23;
																																	case 'PartialNest':
																																		break _v9_26;
																																	case 'DroneNest':
																																		break _v9_26;
																																	case 'NoDroneNest':
																																		break _v9_26;
																																	default:
																																		break _v9_26;
																																}
																															case 'DroneNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_23;
																																	case 'PartialNest':
																																		break _v9_27;
																																	case 'DroneNest':
																																		return _user$project$Model$Win;
																																	case 'NoDroneNest':
																																		break _v9_29;
																																	default:
																																		break _v9_29;
																																}
																															case 'NoDroneNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_23;
																																	case 'PartialNest':
																																		break _v9_27;
																																	case 'DroneNest':
																																		break _v9_29;
																																	case 'NoDroneNest':
																																		break _v9_29;
																																	default:
																																		break _v9_29;
																																}
																															default:
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_23;
																																	case 'PartialNest':
																																		break _v9_27;
																																	case 'DroneNest':
																																		break _v9_29;
																																	case 'NoDroneNest':
																																		break _v9_29;
																																	default:
																																		break _v9_29;
																																}
																														}
																													default:
																														switch (_p10._1.ctor) {
																															case 'EmptyStack':
																																break _v9_1;
																															case 'FullTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_6;
																																	case 'PartialTree':
																																		break _v9_6;
																																	case 'DroneTree':
																																		break _v9_6;
																																	case 'NoDroneTree':
																																		break _v9_6;
																																	case 'FullNest':
																																		break _v9_6;
																																	case 'PartialNest':
																																		break _v9_6;
																																	case 'DroneNest':
																																		break _v9_6;
																																	case 'NoDroneNest':
																																		break _v9_6;
																																	default:
																																		break _v9_6;
																																}
																															case 'PartialTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_10;
																																	case 'DroneTree':
																																		break _v9_10;
																																	case 'NoDroneTree':
																																		break _v9_10;
																																	case 'FullNest':
																																		break _v9_10;
																																	case 'PartialNest':
																																		break _v9_10;
																																	case 'DroneNest':
																																		break _v9_10;
																																	case 'NoDroneNest':
																																		break _v9_10;
																																	default:
																																		break _v9_10;
																																}
																															case 'DroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_14;
																																	case 'NoDroneTree':
																																		break _v9_14;
																																	case 'FullNest':
																																		break _v9_14;
																																	case 'PartialNest':
																																		break _v9_14;
																																	case 'DroneNest':
																																		break _v9_14;
																																	case 'NoDroneNest':
																																		break _v9_14;
																																	default:
																																		break _v9_14;
																																}
																															case 'NoDroneTree':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_18;
																																	case 'FullNest':
																																		break _v9_18;
																																	case 'PartialNest':
																																		break _v9_18;
																																	case 'DroneNest':
																																		break _v9_18;
																																	case 'NoDroneNest':
																																		break _v9_18;
																																	default:
																																		break _v9_18;
																																}
																															case 'FullNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_22;
																																	case 'PartialNest':
																																		break _v9_22;
																																	case 'DroneNest':
																																		break _v9_22;
																																	case 'NoDroneNest':
																																		break _v9_22;
																																	default:
																																		break _v9_22;
																																}
																															case 'PartialNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_23;
																																	case 'PartialNest':
																																		break _v9_26;
																																	case 'DroneNest':
																																		break _v9_26;
																																	case 'NoDroneNest':
																																		break _v9_26;
																																	default:
																																		break _v9_26;
																																}
																															case 'DroneNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_23;
																																	case 'PartialNest':
																																		break _v9_27;
																																	case 'DroneNest':
																																		break _v9_30;
																																	case 'NoDroneNest':
																																		break _v9_30;
																																	default:
																																		break _v9_30;
																																}
																															case 'NoDroneNest':
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_23;
																																	case 'PartialNest':
																																		break _v9_27;
																																	case 'DroneNest':
																																		break _v9_31;
																																	case 'NoDroneNest':
																																		return _user$project$Model$Win;
																																	default:
																																		break _v9_33;
																																}
																															default:
																																switch (_p10._2.ctor) {
																																	case 'EmptyStack':
																																		break _v9_2;
																																	case 'FullTree':
																																		break _v9_7;
																																	case 'PartialTree':
																																		break _v9_11;
																																	case 'DroneTree':
																																		break _v9_15;
																																	case 'NoDroneTree':
																																		break _v9_19;
																																	case 'FullNest':
																																		break _v9_23;
																																	case 'PartialNest':
																																		break _v9_27;
																																	case 'DroneNest':
																																		break _v9_31;
																																	case 'NoDroneNest':
																																		break _v9_33;
																																	default:
																																		break _v9_33;
																																}
																														}
																												}
																											} while(false);
																											return _user$project$Model$Undetermined;
																										} while(false);
																										return _user$project$Model$Undetermined;
																									} while(false);
																									return A4(
																										_user$project$Model$andCheckLine,
																										_user$project$Model$Single(_user$project$Model$Pawn),
																										stack2,
																										stack3,
																										A4(
																											_user$project$Model$andCheckLine,
																											_user$project$Model$Single(_user$project$Model$Drone),
																											stack2,
																											stack3,
																											A3(
																												_user$project$Model$checkLine,
																												_user$project$Model$Single(_user$project$Model$Queen),
																												stack2,
																												stack3)));
																								} while(false);
																								return A4(
																									_user$project$Model$andCheckLine,
																									stack1,
																									_user$project$Model$Single(_user$project$Model$Pawn),
																									stack3,
																									A4(
																										_user$project$Model$andCheckLine,
																										stack1,
																										_user$project$Model$Single(_user$project$Model$Drone),
																										stack3,
																										A3(
																											_user$project$Model$checkLine,
																											stack1,
																											_user$project$Model$Single(_user$project$Model$Queen),
																											stack3)));
																							} while(false);
																							return A4(
																								_user$project$Model$andCheckLine,
																								stack1,
																								stack2,
																								_user$project$Model$Single(_user$project$Model$Pawn),
																								A4(
																									_user$project$Model$andCheckLine,
																									stack1,
																									stack2,
																									_user$project$Model$Single(_user$project$Model$Drone),
																									A3(
																										_user$project$Model$checkLine,
																										stack1,
																										stack2,
																										_user$project$Model$Single(_user$project$Model$Queen))));
																						} while(false);
																						return A4(
																							_user$project$Model$andCheckLine,
																							_user$project$Model$Single(_user$project$Model$Drone),
																							stack2,
																							stack3,
																							A3(
																								_user$project$Model$checkLine,
																								_user$project$Model$Single(_user$project$Model$Queen),
																								stack2,
																								stack3));
																					} while(false);
																					return A4(
																						_user$project$Model$andCheckLine,
																						stack1,
																						_user$project$Model$Single(_user$project$Model$Drone),
																						stack3,
																						A3(
																							_user$project$Model$checkLine,
																							stack1,
																							_user$project$Model$Single(_user$project$Model$Queen),
																							stack3));
																				} while(false);
																				return A4(
																					_user$project$Model$andCheckLine,
																					stack1,
																					stack2,
																					_user$project$Model$Single(_user$project$Model$Drone),
																					A3(
																						_user$project$Model$checkLine,
																						stack1,
																						stack2,
																						_user$project$Model$Single(_user$project$Model$Queen)));
																			} while(false);
																			return A4(
																				_user$project$Model$andCheckLine,
																				_user$project$Model$Single(_user$project$Model$Pawn),
																				stack2,
																				stack3,
																				A3(
																					_user$project$Model$checkLine,
																					_user$project$Model$Single(_user$project$Model$Drone),
																					stack2,
																					stack3));
																		} while(false);
																		return A4(
																			_user$project$Model$andCheckLine,
																			stack1,
																			_user$project$Model$Single(_user$project$Model$Pawn),
																			stack3,
																			A3(
																				_user$project$Model$checkLine,
																				stack1,
																				_user$project$Model$Single(_user$project$Model$Drone),
																				stack3));
																	} while(false);
																	return A4(
																		_user$project$Model$andCheckLine,
																		stack1,
																		stack2,
																		_user$project$Model$Single(_user$project$Model$Pawn),
																		A3(
																			_user$project$Model$checkLine,
																			stack1,
																			stack2,
																			_user$project$Model$Single(_user$project$Model$Drone)));
																} while(false);
																return A4(
																	_user$project$Model$andCheckLine,
																	_user$project$Model$Single(_user$project$Model$Pawn),
																	stack2,
																	stack3,
																	A3(
																		_user$project$Model$checkLine,
																		_user$project$Model$Single(_user$project$Model$Queen),
																		stack2,
																		stack3));
															} while(false);
															return A4(
																_user$project$Model$andCheckLine,
																stack1,
																_user$project$Model$Single(_user$project$Model$Pawn),
																stack3,
																A3(
																	_user$project$Model$checkLine,
																	stack1,
																	_user$project$Model$Single(_user$project$Model$Queen),
																	stack3));
														} while(false);
														return A4(
															_user$project$Model$andCheckLine,
															stack1,
															stack2,
															_user$project$Model$Single(_user$project$Model$Pawn),
															A3(
																_user$project$Model$checkLine,
																stack1,
																stack2,
																_user$project$Model$Single(_user$project$Model$Queen)));
													} while(false);
													var _v13 = _user$project$Model$Single(_user$project$Model$Queen),
														_v14 = stack2,
														_v15 = stack3;
													stack1 = _v13;
													stack2 = _v14;
													stack3 = _v15;
													continue checkLine;
												} while(false);
												var _v16 = stack1,
													_v17 = _user$project$Model$Single(_user$project$Model$Queen),
													_v18 = stack3;
												stack1 = _v16;
												stack2 = _v17;
												stack3 = _v18;
												continue checkLine;
											} while(false);
											var _v19 = stack1,
												_v20 = stack2,
												_v21 = _user$project$Model$Single(_user$project$Model$Queen);
											stack1 = _v19;
											stack2 = _v20;
											stack3 = _v21;
											continue checkLine;
										} while(false);
										var _v22 = _user$project$Model$Single(_user$project$Model$Drone),
											_v23 = stack2,
											_v24 = stack3;
										stack1 = _v22;
										stack2 = _v23;
										stack3 = _v24;
										continue checkLine;
									} while(false);
									var _v25 = stack1,
										_v26 = _user$project$Model$Single(_user$project$Model$Drone),
										_v27 = stack3;
									stack1 = _v25;
									stack2 = _v26;
									stack3 = _v27;
									continue checkLine;
								} while(false);
								var _v28 = stack1,
									_v29 = stack2,
									_v30 = _user$project$Model$Single(_user$project$Model$Drone);
								stack1 = _v28;
								stack2 = _v29;
								stack3 = _v30;
								continue checkLine;
							} while(false);
							var _v31 = _user$project$Model$Single(_user$project$Model$Queen),
								_v32 = stack2,
								_v33 = stack3;
							stack1 = _v31;
							stack2 = _v32;
							stack3 = _v33;
							continue checkLine;
						} while(false);
						var _v34 = stack1,
							_v35 = _user$project$Model$Single(_user$project$Model$Queen),
							_v36 = stack3;
						stack1 = _v34;
						stack2 = _v35;
						stack3 = _v36;
						continue checkLine;
					} while(false);
					var _v37 = stack1,
						_v38 = stack2,
						_v39 = _user$project$Model$Single(_user$project$Model$Queen);
					stack1 = _v37;
					stack2 = _v38;
					stack3 = _v39;
					continue checkLine;
				} while(false);
				var _v40 = _user$project$Model$Single(_user$project$Model$Queen),
					_v41 = stack2,
					_v42 = stack3;
				stack1 = _v40;
				stack2 = _v41;
				stack3 = _v42;
				continue checkLine;
			} while(false);
			var _v43 = stack1,
				_v44 = _user$project$Model$Single(_user$project$Model$Queen),
				_v45 = stack3;
			stack1 = _v43;
			stack2 = _v44;
			stack3 = _v45;
			continue checkLine;
		}
	});
var _user$project$Model$andCheckLine = F4(
	function (stack1, stack2, stack3, outcome) {
		var _p11 = outcome;
		if (_p11.ctor === 'Undetermined') {
			return A3(_user$project$Model$checkLine, stack1, stack2, stack3);
		} else {
			return _p11;
		}
	});
var _user$project$Model$checkForWinner = function (model) {
	var _p12 = model.board;
	switch (_p12.ctor) {
		case 'ThreeByThree':
			var _p13 = _p12._0;
			return A2(
				_user$project$Model$subOutcomeToOutcome,
				model.player,
				A4(
					_user$project$Model$andCheckLine,
					_p13.zeroTwo,
					_p13.oneOne,
					_p13.twoZero,
					A4(
						_user$project$Model$andCheckLine,
						_p13.zeroZero,
						_p13.oneOne,
						_p13.twoTwo,
						A4(
							_user$project$Model$andCheckLine,
							_p13.zeroTwo,
							_p13.oneTwo,
							_p13.twoTwo,
							A4(
								_user$project$Model$andCheckLine,
								_p13.zeroOne,
								_p13.oneOne,
								_p13.twoOne,
								A4(
									_user$project$Model$andCheckLine,
									_p13.zeroZero,
									_p13.oneZero,
									_p13.twoZero,
									A4(
										_user$project$Model$andCheckLine,
										_p13.twoZero,
										_p13.twoOne,
										_p13.twoTwo,
										A4(
											_user$project$Model$andCheckLine,
											_p13.oneZero,
											_p13.oneOne,
											_p13.oneTwo,
											A3(_user$project$Model$checkLine, _p13.zeroZero, _p13.zeroOne, _p13.zeroTwo)))))))));
		case 'ThreeByTwo':
			var _p14 = _p12._0;
			return A2(
				_user$project$Model$subOutcomeToOutcome,
				model.player,
				A4(
					_user$project$Model$andCheckLine,
					_p14.zeroOne,
					_p14.oneOne,
					_p14.twoOne,
					A3(_user$project$Model$checkLine, _p14.zeroZero, _p14.oneZero, _p14.twoZero)));
		case 'TwoByThree':
			var _p15 = _p12._0;
			return A2(
				_user$project$Model$subOutcomeToOutcome,
				model.player,
				A4(
					_user$project$Model$andCheckLine,
					_p15.oneZero,
					_p15.oneOne,
					_p15.oneTwo,
					A3(_user$project$Model$checkLine, _p15.zeroZero, _p15.zeroOne, _p15.zeroTwo)));
		case 'OneByThree':
			return A2(
				_user$project$Model$subOutcomeToOutcome,
				model.player,
				A3(_user$project$Model$checkLine, _p12._0, _p12._1, _p12._2));
		case 'ThreeByOne':
			return A2(
				_user$project$Model$subOutcomeToOutcome,
				model.player,
				A3(_user$project$Model$checkLine, _p12._0, _p12._1, _p12._2));
		default:
			return _user$project$Model$TBD;
	}
};
var _user$project$Model$getOutcome = function (model) {
	var _p16 = _user$project$Model$checkForWinner(model);
	if (_p16.ctor === 'TBD') {
		if (_user$project$Model$stashIsEmpty(model.stash)) {
			return _user$project$Model$Tie;
		} else {
			if (_user$project$Model$noLegalMoves(model)) {
				var _p17 = model.player;
				if (_p17.ctor === 'User') {
					return _user$project$Model$CPUWinByExhaustion;
				} else {
					return _user$project$Model$UserWinByExhaustion;
				}
			} else {
				return _user$project$Model$TBD;
			}
		}
	} else {
		return _p16;
	}
};
var _user$project$Model$placeOnStack = F2(
	function (size, stack) {
		if (A2(_user$project$Model$sizeFits, size, stack)) {
			var _p18 = stack;
			switch (_p18.ctor) {
				case 'EmptyStack':
					return _user$project$Model$Single(size);
				case 'Single':
					var _p19 = {ctor: '_Tuple2', _0: size, _1: _p18._0};
					_v51_6:
					do {
						if (_p19.ctor === '_Tuple2') {
							switch (_p19._0.ctor) {
								case 'Pawn':
									switch (_p19._1.ctor) {
										case 'Drone':
											return _user$project$Model$DroneTree;
										case 'Queen':
											return _user$project$Model$NoDroneTree;
										default:
											break _v51_6;
									}
								case 'Drone':
									switch (_p19._1.ctor) {
										case 'Pawn':
											return _user$project$Model$PartialNest;
										case 'Queen':
											return _user$project$Model$PartialTree;
										default:
											break _v51_6;
									}
								default:
									switch (_p19._1.ctor) {
										case 'Pawn':
											return _user$project$Model$NoDroneNest;
										case 'Drone':
											return _user$project$Model$DroneNest;
										default:
											break _v51_6;
									}
							}
						} else {
							break _v51_6;
						}
					} while(false);
					return stack;
				case 'PartialTree':
					return _user$project$Model$FullTree;
				case 'PartialNest':
					return _user$project$Model$FullNest;
				default:
					return stack;
			}
		} else {
			return stack;
		}
	});
var _user$project$Model$EmptyStack = {ctor: 'EmptyStack'};
var _user$project$Model$get = F2(
	function (boardId, board) {
		var _p20 = board;
		switch (_p20.ctor) {
			case 'EmptyBoard':
				var _p21 = boardId;
				if (_p21.ctor === 'ZeroZero') {
					return _elm_lang$core$Maybe$Just(_user$project$Model$EmptyStack);
				} else {
					return _elm_lang$core$Maybe$Nothing;
				}
			case 'OneByOne':
				var _p22 = boardId;
				if (_p22.ctor === 'ZeroZero') {
					return _elm_lang$core$Maybe$Just(_p20._0);
				} else {
					return _elm_lang$core$Maybe$Nothing;
				}
			case 'OneByTwo':
				var _p23 = boardId;
				switch (_p23.ctor) {
					case 'ZeroZero':
						return _elm_lang$core$Maybe$Just(_p20._0);
					case 'ZeroOne':
						return _elm_lang$core$Maybe$Just(_p20._1);
					default:
						return _elm_lang$core$Maybe$Nothing;
				}
			case 'OneByThree':
				var _p24 = boardId;
				switch (_p24.ctor) {
					case 'ZeroZero':
						return _elm_lang$core$Maybe$Just(_p20._0);
					case 'ZeroOne':
						return _elm_lang$core$Maybe$Just(_p20._1);
					case 'ZeroTwo':
						return _elm_lang$core$Maybe$Just(_p20._2);
					default:
						return _elm_lang$core$Maybe$Nothing;
				}
			case 'TwoByOne':
				var _p25 = boardId;
				switch (_p25.ctor) {
					case 'ZeroZero':
						return _elm_lang$core$Maybe$Just(_p20._0);
					case 'OneZero':
						return _elm_lang$core$Maybe$Just(_p20._1);
					default:
						return _elm_lang$core$Maybe$Nothing;
				}
			case 'ThreeByOne':
				var _p26 = boardId;
				switch (_p26.ctor) {
					case 'ZeroZero':
						return _elm_lang$core$Maybe$Just(_p20._0);
					case 'OneZero':
						return _elm_lang$core$Maybe$Just(_p20._1);
					case 'TwoZero':
						return _elm_lang$core$Maybe$Just(_p20._2);
					default:
						return _elm_lang$core$Maybe$Nothing;
				}
			case 'TwoByTwo':
				var _p28 = _p20._0;
				var _p27 = boardId;
				switch (_p27.ctor) {
					case 'ZeroZero':
						return _elm_lang$core$Maybe$Just(_p28.zeroZero);
					case 'ZeroOne':
						return _elm_lang$core$Maybe$Just(_p28.zeroOne);
					case 'OneZero':
						return _elm_lang$core$Maybe$Just(_p28.oneZero);
					case 'OneOne':
						return _elm_lang$core$Maybe$Just(_p28.oneOne);
					default:
						return _elm_lang$core$Maybe$Nothing;
				}
			case 'TwoByThree':
				var _p30 = _p20._0;
				var _p29 = boardId;
				switch (_p29.ctor) {
					case 'ZeroZero':
						return _elm_lang$core$Maybe$Just(_p30.zeroZero);
					case 'ZeroOne':
						return _elm_lang$core$Maybe$Just(_p30.zeroOne);
					case 'OneZero':
						return _elm_lang$core$Maybe$Just(_p30.oneZero);
					case 'OneOne':
						return _elm_lang$core$Maybe$Just(_p30.oneOne);
					case 'ZeroTwo':
						return _elm_lang$core$Maybe$Just(_p30.zeroTwo);
					case 'OneTwo':
						return _elm_lang$core$Maybe$Just(_p30.oneTwo);
					default:
						return _elm_lang$core$Maybe$Nothing;
				}
			case 'ThreeByTwo':
				var _p32 = _p20._0;
				var _p31 = boardId;
				switch (_p31.ctor) {
					case 'ZeroZero':
						return _elm_lang$core$Maybe$Just(_p32.zeroZero);
					case 'OneZero':
						return _elm_lang$core$Maybe$Just(_p32.oneZero);
					case 'TwoZero':
						return _elm_lang$core$Maybe$Just(_p32.twoZero);
					case 'ZeroOne':
						return _elm_lang$core$Maybe$Just(_p32.zeroOne);
					case 'OneOne':
						return _elm_lang$core$Maybe$Just(_p32.oneOne);
					case 'TwoOne':
						return _elm_lang$core$Maybe$Just(_p32.twoOne);
					default:
						return _elm_lang$core$Maybe$Nothing;
				}
			default:
				var _p34 = _p20._0;
				var _p33 = boardId;
				switch (_p33.ctor) {
					case 'ZeroZero':
						return _elm_lang$core$Maybe$Just(_p34.zeroZero);
					case 'ZeroOne':
						return _elm_lang$core$Maybe$Just(_p34.zeroOne);
					case 'ZeroTwo':
						return _elm_lang$core$Maybe$Just(_p34.zeroTwo);
					case 'OneZero':
						return _elm_lang$core$Maybe$Just(_p34.oneZero);
					case 'OneOne':
						return _elm_lang$core$Maybe$Just(_p34.oneOne);
					case 'OneTwo':
						return _elm_lang$core$Maybe$Just(_p34.oneTwo);
					case 'TwoZero':
						return _elm_lang$core$Maybe$Just(_p34.twoZero);
					case 'TwoOne':
						return _elm_lang$core$Maybe$Just(_p34.twoOne);
					default:
						return _elm_lang$core$Maybe$Just(_p34.twoTwo);
				}
		}
	});
var _user$project$Model$ThreeByThree = function (a) {
	return {ctor: 'ThreeByThree', _0: a};
};
var _user$project$Model$threeByThree = F9(
	function (zeroZero, oneZero, twoZero, zeroOne, oneOne, twoOne, zeroTwo, oneTwo, twoTwo) {
		return _user$project$Model$ThreeByThree(
			{zeroZero: zeroZero, zeroOne: zeroOne, zeroTwo: zeroTwo, oneZero: oneZero, oneOne: oneOne, oneTwo: oneTwo, twoZero: twoZero, twoOne: twoOne, twoTwo: twoTwo});
	});
var _user$project$Model$ThreeByTwo = function (a) {
	return {ctor: 'ThreeByTwo', _0: a};
};
var _user$project$Model$threeByTwo = F6(
	function (zeroZero, oneZero, twoZero, zeroOne, oneOne, twoOne) {
		return _user$project$Model$ThreeByTwo(
			{zeroZero: zeroZero, oneZero: oneZero, twoZero: twoZero, zeroOne: zeroOne, oneOne: oneOne, twoOne: twoOne});
	});
var _user$project$Model$TwoByThree = function (a) {
	return {ctor: 'TwoByThree', _0: a};
};
var _user$project$Model$twoByThree = F6(
	function (zeroZero, oneZero, zeroOne, oneOne, zeroTwo, oneTwo) {
		return _user$project$Model$TwoByThree(
			{zeroZero: zeroZero, oneZero: oneZero, zeroOne: zeroOne, oneOne: oneOne, zeroTwo: zeroTwo, oneTwo: oneTwo});
	});
var _user$project$Model$TwoByTwo = function (a) {
	return {ctor: 'TwoByTwo', _0: a};
};
var _user$project$Model$twoByTwo = F4(
	function (zeroZero, oneZero, zeroOne, oneOne) {
		return _user$project$Model$TwoByTwo(
			{zeroZero: zeroZero, oneZero: oneZero, zeroOne: zeroOne, oneOne: oneOne});
	});
var _user$project$Model$ThreeByOne = F3(
	function (a, b, c) {
		return {ctor: 'ThreeByOne', _0: a, _1: b, _2: c};
	});
var _user$project$Model$OneByThree = F3(
	function (a, b, c) {
		return {ctor: 'OneByThree', _0: a, _1: b, _2: c};
	});
var _user$project$Model$TwoByOne = F2(
	function (a, b) {
		return {ctor: 'TwoByOne', _0: a, _1: b};
	});
var _user$project$Model$OneByTwo = F2(
	function (a, b) {
		return {ctor: 'OneByTwo', _0: a, _1: b};
	});
var _user$project$Model$placeOnEdge = F3(
	function (boardId, size, board) {
		var _p35 = board;
		switch (_p35.ctor) {
			case 'EmptyBoard':
				return board;
			case 'OneByOne':
				var _p37 = _p35._0;
				var _p36 = boardId;
				switch (_p36.ctor) {
					case 'EdgeZeroZero':
						return A4(
							_user$project$Model$twoByTwo,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_p37);
					case 'EdgeOneZero':
						return A2(
							_user$project$Model$OneByTwo,
							_user$project$Model$Single(size),
							_p37);
					case 'EdgeTwoZero':
						return A4(
							_user$project$Model$twoByTwo,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size),
							_p37,
							_user$project$Model$EmptyStack);
					case 'EdgeZeroOne':
						return A2(
							_user$project$Model$TwoByOne,
							_user$project$Model$Single(size),
							_p37);
					case 'EdgeTwoOne':
						return A2(
							_user$project$Model$TwoByOne,
							_p37,
							_user$project$Model$Single(size));
					case 'EdgeZeroTwo':
						return A4(
							_user$project$Model$twoByTwo,
							_user$project$Model$EmptyStack,
							_p37,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack);
					case 'EdgeOneTwo':
						return A2(
							_user$project$Model$OneByTwo,
							_p37,
							_user$project$Model$Single(size));
					case 'EdgeTwoTwo':
						return A4(
							_user$project$Model$twoByTwo,
							_p37,
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size));
					default:
						return board;
				}
			case 'OneByTwo':
				var _p40 = _p35._1;
				var _p39 = _p35._0;
				var _p38 = boardId;
				switch (_p38.ctor) {
					case 'EdgeZeroZero':
						return A6(
							_user$project$Model$twoByThree,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_p39,
							_user$project$Model$EmptyStack,
							_p40);
					case 'EdgeOneZero':
						return A3(
							_user$project$Model$OneByThree,
							_user$project$Model$Single(size),
							_p39,
							_p40);
					case 'EdgeTwoZero':
						return A6(
							_user$project$Model$twoByThree,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size),
							_p39,
							_user$project$Model$EmptyStack,
							_p40,
							_user$project$Model$EmptyStack);
					case 'EdgeZeroOne':
						return A4(
							_user$project$Model$twoByTwo,
							_user$project$Model$Single(size),
							_p39,
							_user$project$Model$EmptyStack,
							_p40);
					case 'EdgeTwoOne':
						return A4(
							_user$project$Model$twoByTwo,
							_p39,
							_user$project$Model$Single(size),
							_p40,
							_user$project$Model$EmptyStack);
					case 'EdgeZeroTwo':
						return A4(
							_user$project$Model$twoByTwo,
							_user$project$Model$EmptyStack,
							_p39,
							_user$project$Model$Single(size),
							_p40);
					case 'EdgeTwoTwo':
						return A4(
							_user$project$Model$twoByTwo,
							_p39,
							_user$project$Model$EmptyStack,
							_p40,
							_user$project$Model$Single(size));
					case 'EdgeZeroThree':
						return A6(
							_user$project$Model$twoByThree,
							_user$project$Model$EmptyStack,
							_p39,
							_user$project$Model$EmptyStack,
							_p40,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack);
					case 'EdgeOneThree':
						return A3(
							_user$project$Model$OneByThree,
							_p39,
							_p40,
							_user$project$Model$Single(size));
					case 'EdgeTwoThree':
						return A6(
							_user$project$Model$twoByThree,
							_p39,
							_user$project$Model$EmptyStack,
							_p40,
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size));
					default:
						return board;
				}
			case 'TwoByOne':
				var _p43 = _p35._1;
				var _p42 = _p35._0;
				var _p41 = boardId;
				switch (_p41.ctor) {
					case 'EdgeZeroZero':
						return A6(
							_user$project$Model$threeByTwo,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_p42,
							_p43);
					case 'EdgeOneZero':
						return A4(
							_user$project$Model$twoByTwo,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack,
							_p42,
							_p43);
					case 'EdgeTwoZero':
						return A4(
							_user$project$Model$twoByTwo,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size),
							_p42,
							_p43);
					case 'EdgeThreeZero':
						return A6(
							_user$project$Model$threeByTwo,
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size),
							_p42,
							_p43,
							_user$project$Model$EmptyStack);
					case 'EdgeZeroOne':
						return A3(
							_user$project$Model$ThreeByOne,
							_user$project$Model$Single(size),
							_p42,
							_p43);
					case 'EdgeThreeOne':
						return A3(
							_user$project$Model$ThreeByOne,
							_p42,
							_p43,
							_user$project$Model$Single(size));
					case 'EdgeZeroTwo':
						return A6(
							_user$project$Model$threeByTwo,
							_user$project$Model$EmptyStack,
							_p42,
							_p43,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack);
					case 'EdgeOneTwo':
						return A4(
							_user$project$Model$twoByTwo,
							_p42,
							_p43,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack);
					case 'EdgeTwoTwo':
						return A4(
							_user$project$Model$twoByTwo,
							_p42,
							_p43,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size));
					case 'EdgeThreeTwo':
						return A6(
							_user$project$Model$threeByTwo,
							_p42,
							_p43,
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size));
					default:
						return board;
				}
			case 'OneByThree':
				var _p47 = _p35._2;
				var _p46 = _p35._1;
				var _p45 = _p35._0;
				var _p44 = boardId;
				switch (_p44.ctor) {
					case 'EdgeZeroZero':
						return A6(
							_user$project$Model$twoByThree,
							_user$project$Model$Single(size),
							_p45,
							_user$project$Model$EmptyStack,
							_p46,
							_user$project$Model$EmptyStack,
							_p47);
					case 'EdgeTwoZero':
						return A6(
							_user$project$Model$twoByThree,
							_p45,
							_user$project$Model$Single(size),
							_p46,
							_user$project$Model$EmptyStack,
							_p47,
							_user$project$Model$EmptyStack);
					case 'EdgeZeroOne':
						return A6(
							_user$project$Model$twoByThree,
							_user$project$Model$EmptyStack,
							_p45,
							_user$project$Model$Single(size),
							_p46,
							_user$project$Model$EmptyStack,
							_p47);
					case 'EdgeTwoOne':
						return A6(
							_user$project$Model$twoByThree,
							_p45,
							_user$project$Model$EmptyStack,
							_p46,
							_user$project$Model$Single(size),
							_p47,
							_user$project$Model$EmptyStack);
					case 'EdgeZeroTwo':
						return A6(
							_user$project$Model$twoByThree,
							_user$project$Model$EmptyStack,
							_p45,
							_user$project$Model$EmptyStack,
							_p46,
							_user$project$Model$Single(size),
							_p47);
					case 'EdgeTwoTwo':
						return A6(
							_user$project$Model$twoByThree,
							_p45,
							_user$project$Model$EmptyStack,
							_p46,
							_user$project$Model$EmptyStack,
							_p47,
							_user$project$Model$Single(size));
					default:
						return board;
				}
			case 'ThreeByOne':
				var _p51 = _p35._2;
				var _p50 = _p35._1;
				var _p49 = _p35._0;
				var _p48 = boardId;
				switch (_p48.ctor) {
					case 'EdgeZeroZero':
						return A6(
							_user$project$Model$threeByTwo,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_p49,
							_p50,
							_p51);
					case 'EdgeOneZero':
						return A6(
							_user$project$Model$threeByTwo,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack,
							_p49,
							_p50,
							_p51);
					case 'EdgeTwoZero':
						return A6(
							_user$project$Model$threeByTwo,
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size),
							_p49,
							_p50,
							_p51);
					case 'EdgeZeroTwo':
						return A6(
							_user$project$Model$threeByTwo,
							_p49,
							_p50,
							_p51,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack);
					case 'EdgeOneTwo':
						return A6(
							_user$project$Model$threeByTwo,
							_p49,
							_p50,
							_p51,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack);
					case 'EdgeTwoTwo':
						return A6(
							_user$project$Model$threeByTwo,
							_p49,
							_p50,
							_p51,
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size));
					default:
						return board;
				}
			case 'TwoByTwo':
				var _p53 = _p35._0;
				var s4 = _p53.oneOne;
				var s3 = _p53.zeroOne;
				var s2 = _p53.oneZero;
				var s1 = _p53.zeroZero;
				var _p52 = boardId;
				switch (_p52.ctor) {
					case 'EdgeZeroZero':
						return A9(
							_user$project$Model$threeByThree,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							s1,
							s2,
							_user$project$Model$EmptyStack,
							s3,
							s4);
					case 'EdgeOneZero':
						return A6(
							_user$project$Model$twoByThree,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack,
							s1,
							s2,
							s3,
							s4);
					case 'EdgeTwoZero':
						return A6(
							_user$project$Model$twoByThree,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size),
							s1,
							s2,
							s3,
							s4);
					case 'EdgeThreeZero':
						return A9(
							_user$project$Model$threeByThree,
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size),
							s1,
							s2,
							_user$project$Model$EmptyStack,
							s3,
							s4,
							_user$project$Model$EmptyStack);
					case 'EdgeZeroOne':
						return A6(
							_user$project$Model$threeByTwo,
							_user$project$Model$Single(size),
							s1,
							s2,
							_user$project$Model$EmptyStack,
							s3,
							s4);
					case 'EdgeThreeOne':
						return A6(
							_user$project$Model$threeByTwo,
							s1,
							s2,
							_user$project$Model$Single(size),
							s3,
							s4,
							_user$project$Model$EmptyStack);
					case 'EdgeZeroTwo':
						return A6(
							_user$project$Model$threeByTwo,
							_user$project$Model$EmptyStack,
							s1,
							s2,
							_user$project$Model$Single(size),
							s3,
							s4);
					case 'EdgeThreeTwo':
						return A6(
							_user$project$Model$threeByTwo,
							s1,
							s2,
							_user$project$Model$EmptyStack,
							s3,
							s4,
							_user$project$Model$Single(size));
					case 'EdgeZeroThree':
						return A9(
							_user$project$Model$threeByThree,
							_user$project$Model$EmptyStack,
							s1,
							s2,
							_user$project$Model$EmptyStack,
							s3,
							s4,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack);
					case 'EdgeOneThree':
						return A6(
							_user$project$Model$twoByThree,
							s1,
							s2,
							s3,
							s4,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack);
					case 'EdgeTwoThree':
						return A6(
							_user$project$Model$twoByThree,
							s1,
							s2,
							s3,
							s4,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size));
					case 'EdgeThreeThree':
						return A9(
							_user$project$Model$threeByThree,
							s1,
							s2,
							_user$project$Model$EmptyStack,
							s3,
							s4,
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size));
					default:
						return board;
				}
			case 'TwoByThree':
				var _p55 = _p35._0;
				var s6 = _p55.oneTwo;
				var s5 = _p55.zeroTwo;
				var s4 = _p55.oneOne;
				var s3 = _p55.zeroOne;
				var s2 = _p55.oneZero;
				var s1 = _p55.zeroZero;
				var _p54 = boardId;
				switch (_p54.ctor) {
					case 'EdgeZeroZero':
						return A9(
							_user$project$Model$threeByThree,
							_user$project$Model$Single(size),
							s1,
							s2,
							_user$project$Model$EmptyStack,
							s3,
							s4,
							_user$project$Model$EmptyStack,
							s5,
							s6);
					case 'EdgeThreeZero':
						return A9(
							_user$project$Model$threeByThree,
							s1,
							s2,
							_user$project$Model$Single(size),
							s3,
							s4,
							_user$project$Model$EmptyStack,
							s5,
							s6,
							_user$project$Model$EmptyStack);
					case 'EdgeZeroOne':
						return A9(
							_user$project$Model$threeByThree,
							_user$project$Model$EmptyStack,
							s1,
							s2,
							_user$project$Model$Single(size),
							s3,
							s4,
							_user$project$Model$EmptyStack,
							s5,
							s6);
					case 'EdgeThreeOne':
						return A9(
							_user$project$Model$threeByThree,
							s1,
							s2,
							_user$project$Model$EmptyStack,
							s3,
							s4,
							_user$project$Model$Single(size),
							s5,
							s6,
							_user$project$Model$EmptyStack);
					case 'EdgeZeroTwo':
						return A9(
							_user$project$Model$threeByThree,
							_user$project$Model$EmptyStack,
							s1,
							s2,
							_user$project$Model$EmptyStack,
							s3,
							s4,
							_user$project$Model$Single(size),
							s5,
							s6);
					case 'EdgeThreeTwo':
						return A9(
							_user$project$Model$threeByThree,
							s1,
							s2,
							_user$project$Model$EmptyStack,
							s3,
							s4,
							_user$project$Model$EmptyStack,
							s5,
							s6,
							_user$project$Model$Single(size));
					default:
						return board;
				}
			case 'ThreeByTwo':
				var _p57 = _p35._0;
				var s6 = _p57.twoOne;
				var s5 = _p57.oneOne;
				var s4 = _p57.zeroOne;
				var s3 = _p57.twoZero;
				var s2 = _p57.oneZero;
				var s1 = _p57.zeroZero;
				var _p56 = boardId;
				switch (_p56.ctor) {
					case 'EdgeZeroZero':
						return A9(
							_user$project$Model$threeByThree,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							s1,
							s2,
							s3,
							s4,
							s5,
							s6);
					case 'EdgeOneZero':
						return A9(
							_user$project$Model$threeByThree,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack,
							s1,
							s2,
							s3,
							s4,
							s5,
							s6);
					case 'EdgeTwoZero':
						return A9(
							_user$project$Model$threeByThree,
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size),
							s1,
							s2,
							s3,
							s4,
							s5,
							s6);
					case 'EdgeZeroThree':
						return A9(
							_user$project$Model$threeByThree,
							s1,
							s2,
							s3,
							s4,
							s5,
							s6,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack);
					case 'EdgeOneThree':
						return A9(
							_user$project$Model$threeByThree,
							s1,
							s2,
							s3,
							s4,
							s5,
							s6,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size),
							_user$project$Model$EmptyStack);
					case 'EdgeTwoThree':
						return A9(
							_user$project$Model$threeByThree,
							s1,
							s2,
							s3,
							s4,
							s5,
							s6,
							_user$project$Model$EmptyStack,
							_user$project$Model$EmptyStack,
							_user$project$Model$Single(size));
					default:
						return board;
				}
			default:
				return board;
		}
	});
var _user$project$Model$OneByOne = function (a) {
	return {ctor: 'OneByOne', _0: a};
};
var _user$project$Model$set = F3(
	function (boardId, stack, board) {
		var _p58 = board;
		switch (_p58.ctor) {
			case 'EmptyBoard':
				var _p59 = boardId;
				if (_p59.ctor === 'ZeroZero') {
					return _user$project$Model$OneByOne(stack);
				} else {
					return board;
				}
			case 'OneByOne':
				var _p60 = boardId;
				if (_p60.ctor === 'ZeroZero') {
					return _user$project$Model$OneByOne(stack);
				} else {
					return board;
				}
			case 'OneByTwo':
				var _p61 = boardId;
				switch (_p61.ctor) {
					case 'ZeroZero':
						return A2(_user$project$Model$OneByTwo, stack, _p58._1);
					case 'ZeroOne':
						return A2(_user$project$Model$OneByTwo, _p58._0, stack);
					default:
						return board;
				}
			case 'TwoByOne':
				var _p62 = boardId;
				switch (_p62.ctor) {
					case 'ZeroZero':
						return A2(_user$project$Model$TwoByOne, stack, _p58._1);
					case 'OneZero':
						return A2(_user$project$Model$TwoByOne, _p58._0, stack);
					default:
						return board;
				}
			case 'OneByThree':
				var _p66 = _p58._2;
				var _p65 = _p58._1;
				var _p64 = _p58._0;
				var _p63 = boardId;
				switch (_p63.ctor) {
					case 'ZeroZero':
						return A3(_user$project$Model$OneByThree, stack, _p65, _p66);
					case 'ZeroOne':
						return A3(_user$project$Model$OneByThree, _p64, stack, _p66);
					case 'ZeroTwo':
						return A3(_user$project$Model$OneByThree, _p64, _p65, stack);
					default:
						return board;
				}
			case 'ThreeByOne':
				var _p70 = _p58._2;
				var _p69 = _p58._1;
				var _p68 = _p58._0;
				var _p67 = boardId;
				switch (_p67.ctor) {
					case 'ZeroZero':
						return A3(_user$project$Model$ThreeByOne, stack, _p69, _p70);
					case 'OneZero':
						return A3(_user$project$Model$ThreeByOne, _p68, stack, _p70);
					case 'TwoZero':
						return A3(_user$project$Model$ThreeByOne, _p68, _p69, stack);
					default:
						return board;
				}
			case 'TwoByTwo':
				var _p72 = _p58._0;
				var _p71 = boardId;
				switch (_p71.ctor) {
					case 'ZeroZero':
						return _user$project$Model$TwoByTwo(
							_elm_lang$core$Native_Utils.update(
								_p72,
								{zeroZero: stack}));
					case 'ZeroOne':
						return _user$project$Model$TwoByTwo(
							_elm_lang$core$Native_Utils.update(
								_p72,
								{zeroOne: stack}));
					case 'OneZero':
						return _user$project$Model$TwoByTwo(
							_elm_lang$core$Native_Utils.update(
								_p72,
								{oneZero: stack}));
					case 'OneOne':
						return _user$project$Model$TwoByTwo(
							_elm_lang$core$Native_Utils.update(
								_p72,
								{oneOne: stack}));
					default:
						return board;
				}
			case 'TwoByThree':
				var _p74 = _p58._0;
				var _p73 = boardId;
				switch (_p73.ctor) {
					case 'ZeroZero':
						return _user$project$Model$TwoByThree(
							_elm_lang$core$Native_Utils.update(
								_p74,
								{zeroZero: stack}));
					case 'ZeroOne':
						return _user$project$Model$TwoByThree(
							_elm_lang$core$Native_Utils.update(
								_p74,
								{zeroOne: stack}));
					case 'OneZero':
						return _user$project$Model$TwoByThree(
							_elm_lang$core$Native_Utils.update(
								_p74,
								{oneZero: stack}));
					case 'OneOne':
						return _user$project$Model$TwoByThree(
							_elm_lang$core$Native_Utils.update(
								_p74,
								{oneOne: stack}));
					case 'ZeroTwo':
						return _user$project$Model$TwoByThree(
							_elm_lang$core$Native_Utils.update(
								_p74,
								{zeroTwo: stack}));
					case 'OneTwo':
						return _user$project$Model$TwoByThree(
							_elm_lang$core$Native_Utils.update(
								_p74,
								{oneTwo: stack}));
					default:
						return board;
				}
			case 'ThreeByTwo':
				var _p76 = _p58._0;
				var _p75 = boardId;
				switch (_p75.ctor) {
					case 'ZeroZero':
						return _user$project$Model$ThreeByTwo(
							_elm_lang$core$Native_Utils.update(
								_p76,
								{zeroZero: stack}));
					case 'OneZero':
						return _user$project$Model$ThreeByTwo(
							_elm_lang$core$Native_Utils.update(
								_p76,
								{oneZero: stack}));
					case 'TwoZero':
						return _user$project$Model$ThreeByTwo(
							_elm_lang$core$Native_Utils.update(
								_p76,
								{twoZero: stack}));
					case 'ZeroOne':
						return _user$project$Model$ThreeByTwo(
							_elm_lang$core$Native_Utils.update(
								_p76,
								{zeroOne: stack}));
					case 'OneOne':
						return _user$project$Model$ThreeByTwo(
							_elm_lang$core$Native_Utils.update(
								_p76,
								{oneOne: stack}));
					case 'TwoOne':
						return _user$project$Model$ThreeByTwo(
							_elm_lang$core$Native_Utils.update(
								_p76,
								{twoOne: stack}));
					default:
						return board;
				}
			default:
				var _p78 = _p58._0;
				var _p77 = boardId;
				switch (_p77.ctor) {
					case 'ZeroZero':
						return _user$project$Model$ThreeByThree(
							_elm_lang$core$Native_Utils.update(
								_p78,
								{zeroZero: stack}));
					case 'ZeroOne':
						return _user$project$Model$ThreeByThree(
							_elm_lang$core$Native_Utils.update(
								_p78,
								{zeroOne: stack}));
					case 'ZeroTwo':
						return _user$project$Model$ThreeByThree(
							_elm_lang$core$Native_Utils.update(
								_p78,
								{zeroTwo: stack}));
					case 'OneZero':
						return _user$project$Model$ThreeByThree(
							_elm_lang$core$Native_Utils.update(
								_p78,
								{oneZero: stack}));
					case 'OneOne':
						return _user$project$Model$ThreeByThree(
							_elm_lang$core$Native_Utils.update(
								_p78,
								{oneOne: stack}));
					case 'OneTwo':
						return _user$project$Model$ThreeByThree(
							_elm_lang$core$Native_Utils.update(
								_p78,
								{oneTwo: stack}));
					case 'TwoZero':
						return _user$project$Model$ThreeByThree(
							_elm_lang$core$Native_Utils.update(
								_p78,
								{twoZero: stack}));
					case 'TwoOne':
						return _user$project$Model$ThreeByThree(
							_elm_lang$core$Native_Utils.update(
								_p78,
								{twoOne: stack}));
					default:
						return _user$project$Model$ThreeByThree(
							_elm_lang$core$Native_Utils.update(
								_p78,
								{twoTwo: stack}));
				}
		}
	});
var _user$project$Model$place = F3(
	function (boardId, size, board) {
		var _p79 = A2(_user$project$Model$get, boardId, board);
		if (_p79.ctor === 'Just') {
			return A3(
				_user$project$Model$set,
				boardId,
				A2(_user$project$Model$placeOnStack, size, _p79._0),
				board);
		} else {
			return board;
		}
	});
var _user$project$Model$EmptyBoard = {ctor: 'EmptyBoard'};
var _user$project$Model$defaultState = {
	mdl: _debois$elm_mdl$Material$model,
	selected: _elm_lang$core$Maybe$Just(_user$project$Model$Pawn),
	board: _user$project$Model$EmptyBoard,
	stash: _user$project$Model$defaultStash,
	player: _user$project$Model$User,
	outcome: _user$project$Model$TBD
};
var _user$project$Model$TwoTwo = {ctor: 'TwoTwo'};
var _user$project$Model$TwoOne = {ctor: 'TwoOne'};
var _user$project$Model$TwoZero = {ctor: 'TwoZero'};
var _user$project$Model$OneTwo = {ctor: 'OneTwo'};
var _user$project$Model$OneOne = {ctor: 'OneOne'};
var _user$project$Model$OneZero = {ctor: 'OneZero'};
var _user$project$Model$ZeroTwo = {ctor: 'ZeroTwo'};
var _user$project$Model$ZeroOne = {ctor: 'ZeroOne'};
var _user$project$Model$ZeroZero = {ctor: 'ZeroZero'};
var _user$project$Model$getPotentialBoardIds = function (board) {
	var _p80 = board;
	switch (_p80.ctor) {
		case 'EmptyBoard':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$ZeroZero]);
		case 'OneByOne':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$ZeroZero]);
		case 'OneByTwo':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$ZeroZero, _user$project$Model$ZeroOne]);
		case 'TwoByOne':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$ZeroZero, _user$project$Model$OneZero]);
		case 'OneByThree':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$ZeroZero, _user$project$Model$ZeroOne, _user$project$Model$ZeroTwo]);
		case 'ThreeByOne':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$ZeroZero, _user$project$Model$OneZero, _user$project$Model$TwoZero]);
		case 'TwoByTwo':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$ZeroZero, _user$project$Model$ZeroOne, _user$project$Model$OneZero, _user$project$Model$OneOne]);
		case 'TwoByThree':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$ZeroZero, _user$project$Model$OneZero, _user$project$Model$ZeroOne, _user$project$Model$OneOne, _user$project$Model$ZeroTwo, _user$project$Model$OneTwo]);
		case 'ThreeByTwo':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$ZeroZero, _user$project$Model$OneZero, _user$project$Model$TwoZero, _user$project$Model$ZeroOne, _user$project$Model$OneOne, _user$project$Model$TwoOne]);
		default:
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$ZeroZero, _user$project$Model$OneZero, _user$project$Model$TwoZero, _user$project$Model$ZeroOne, _user$project$Model$OneOne, _user$project$Model$TwoOne, _user$project$Model$ZeroTwo, _user$project$Model$OneTwo, _user$project$Model$TwoTwo]);
	}
};
var _user$project$Model$getAvailableBoardIdSizePairs = F2(
	function (board, stash) {
		var potentialBoardIds = _user$project$Model$getPotentialBoardIds(board);
		var availableSizes = _user$project$Model$getAvailableSizes(stash);
		return A2(
			_elm_lang$core$List$concatMap,
			function (boardId) {
				var _p81 = A2(_user$project$Model$get, boardId, board);
				if (_p81.ctor === 'Just') {
					return A2(
						_elm_lang$core$List$filterMap,
						function (size) {
							return A2(_user$project$Model$sizeFits, size, _p81._0) ? _elm_lang$core$Maybe$Just(
								{ctor: '_Tuple2', _0: boardId, _1: size}) : _elm_lang$core$Maybe$Nothing;
						},
						availableSizes);
				} else {
					return _elm_lang$core$Native_List.fromArray(
						[]);
				}
			},
			potentialBoardIds);
	});
var _user$project$Model$EdgeThreeThree = {ctor: 'EdgeThreeThree'};
var _user$project$Model$EdgeThreeTwo = {ctor: 'EdgeThreeTwo'};
var _user$project$Model$EdgeThreeOne = {ctor: 'EdgeThreeOne'};
var _user$project$Model$EdgeThreeZero = {ctor: 'EdgeThreeZero'};
var _user$project$Model$EdgeTwoThree = {ctor: 'EdgeTwoThree'};
var _user$project$Model$EdgeTwoTwo = {ctor: 'EdgeTwoTwo'};
var _user$project$Model$EdgeTwoOne = {ctor: 'EdgeTwoOne'};
var _user$project$Model$EdgeTwoZero = {ctor: 'EdgeTwoZero'};
var _user$project$Model$EdgeOneThree = {ctor: 'EdgeOneThree'};
var _user$project$Model$EdgeOneTwo = {ctor: 'EdgeOneTwo'};
var _user$project$Model$EdgeOneOne = {ctor: 'EdgeOneOne'};
var _user$project$Model$EdgeOneZero = {ctor: 'EdgeOneZero'};
var _user$project$Model$EdgeZeroThree = {ctor: 'EdgeZeroThree'};
var _user$project$Model$EdgeZeroTwo = {ctor: 'EdgeZeroTwo'};
var _user$project$Model$EdgeZeroOne = {ctor: 'EdgeZeroOne'};
var _user$project$Model$EdgeZeroZero = {ctor: 'EdgeZeroZero'};
var _user$project$Model$getAvailableEdgeIds = function (board) {
	var _p82 = board;
	switch (_p82.ctor) {
		case 'EmptyBoard':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$EdgeZeroZero]);
		case 'OneByOne':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$EdgeZeroZero, _user$project$Model$EdgeOneZero, _user$project$Model$EdgeTwoZero, _user$project$Model$EdgeZeroOne, _user$project$Model$EdgeTwoOne, _user$project$Model$EdgeZeroTwo, _user$project$Model$EdgeOneTwo, _user$project$Model$EdgeTwoTwo]);
		case 'OneByTwo':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$EdgeZeroZero, _user$project$Model$EdgeOneZero, _user$project$Model$EdgeTwoZero, _user$project$Model$EdgeZeroOne, _user$project$Model$EdgeTwoOne, _user$project$Model$EdgeZeroTwo, _user$project$Model$EdgeTwoTwo, _user$project$Model$EdgeZeroThree, _user$project$Model$EdgeOneThree, _user$project$Model$EdgeTwoThree]);
		case 'TwoByOne':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$EdgeZeroZero, _user$project$Model$EdgeOneZero, _user$project$Model$EdgeTwoZero, _user$project$Model$EdgeThreeZero, _user$project$Model$EdgeZeroOne, _user$project$Model$EdgeThreeOne, _user$project$Model$EdgeZeroTwo, _user$project$Model$EdgeOneTwo, _user$project$Model$EdgeTwoTwo, _user$project$Model$EdgeThreeTwo]);
		case 'OneByThree':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$EdgeZeroZero, _user$project$Model$EdgeTwoZero, _user$project$Model$EdgeZeroOne, _user$project$Model$EdgeTwoOne, _user$project$Model$EdgeZeroTwo, _user$project$Model$EdgeTwoTwo]);
		case 'ThreeByOne':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$EdgeZeroZero, _user$project$Model$EdgeOneZero, _user$project$Model$EdgeTwoZero, _user$project$Model$EdgeZeroTwo, _user$project$Model$EdgeOneTwo, _user$project$Model$EdgeTwoTwo]);
		case 'TwoByTwo':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$EdgeZeroZero, _user$project$Model$EdgeOneZero, _user$project$Model$EdgeTwoZero, _user$project$Model$EdgeThreeZero, _user$project$Model$EdgeZeroOne, _user$project$Model$EdgeThreeOne, _user$project$Model$EdgeZeroTwo, _user$project$Model$EdgeThreeTwo, _user$project$Model$EdgeZeroThree, _user$project$Model$EdgeOneThree, _user$project$Model$EdgeTwoThree, _user$project$Model$EdgeThreeThree]);
		case 'TwoByThree':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$EdgeZeroZero, _user$project$Model$EdgeThreeZero, _user$project$Model$EdgeZeroOne, _user$project$Model$EdgeThreeOne, _user$project$Model$EdgeZeroTwo, _user$project$Model$EdgeThreeTwo]);
		case 'ThreeByTwo':
			return _elm_lang$core$Native_List.fromArray(
				[_user$project$Model$EdgeZeroZero, _user$project$Model$EdgeOneZero, _user$project$Model$EdgeTwoZero, _user$project$Model$EdgeZeroThree, _user$project$Model$EdgeOneThree, _user$project$Model$EdgeTwoThree]);
		default:
			return _elm_lang$core$Native_List.fromArray(
				[]);
	}
};
var _user$project$Model$getAvailableEdgeIdSizePairs = F2(
	function (board, stash) {
		var availableEdgeIds = _user$project$Model$getAvailableEdgeIds(board);
		var availableSizes = _user$project$Model$getAvailableSizes(stash);
		return A2(
			_elm_lang$core$List$concatMap,
			function (edgeId) {
				return A2(
					_elm_lang$core$List$map,
					F2(
						function (v0, v1) {
							return {ctor: '_Tuple2', _0: v0, _1: v1};
						})(edgeId),
					availableSizes);
			},
			availableEdgeIds);
	});

var _user$project$Msg$NewGame = {ctor: 'NewGame'};
var _user$project$Msg$Select = function (a) {
	return {ctor: 'Select', _0: a};
};
var _user$project$Msg$PlaceOnEdge = function (a) {
	return {ctor: 'PlaceOnEdge', _0: a};
};
var _user$project$Msg$Place = function (a) {
	return {ctor: 'Place', _0: a};
};
var _user$project$Msg$NoOp = {ctor: 'NoOp'};
var _user$project$Msg$Mdl = function (a) {
	return {ctor: 'Mdl', _0: a};
};

var _user$project$PyramidView$nullSvg = A2(
	_elm_lang$svg$Svg$polygon,
	_elm_lang$core$Native_List.fromArray(
		[]),
	_elm_lang$core$Native_List.fromArray(
		[]));
var _user$project$PyramidView$pyramidHeightConstant = 3;
var _user$project$PyramidView$pyramidPathSuffix = function (scale) {
	var doubleSclaeString = _elm_lang$core$Basics$toString(2 * scale);
	var sclaeString = _elm_lang$core$Basics$toString(scale);
	return A2(
		_elm_lang$core$Basics_ops['++'],
		A2(
			_elm_lang$core$Basics_ops['++'],
			' l ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				sclaeString,
				A2(_elm_lang$core$Basics_ops['++'], ' -', sclaeString))),
		A2(
			_elm_lang$core$Basics_ops['++'],
			A2(
				_elm_lang$core$Basics_ops['++'],
				'l -',
				A2(
					_elm_lang$core$Basics_ops['++'],
					sclaeString,
					A2(_elm_lang$core$Basics_ops['++'], ' -', doubleSclaeString))),
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(
					_elm_lang$core$Basics_ops['++'],
					' l -',
					A2(
						_elm_lang$core$Basics_ops['++'],
						sclaeString,
						A2(_elm_lang$core$Basics_ops['++'], ' ', doubleSclaeString))),
				A2(
					_elm_lang$core$Basics_ops['++'],
					A2(
						_elm_lang$core$Basics_ops['++'],
						' l ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							sclaeString,
							A2(_elm_lang$core$Basics_ops['++'], ' ', sclaeString))),
					A2(
						_elm_lang$core$Basics_ops['++'],
						' Z ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							' l 0 ',
							_elm_lang$core$Basics$toString((0 - _user$project$PyramidView$pyramidHeightConstant) * scale)))))));
};
var _user$project$PyramidView$drone2ndY = 11 / 10;
var _user$project$PyramidView$drone2ndX = 3 / 4;
var _user$project$PyramidView$queen3rdY = 19 / 16;
var _user$project$PyramidView$queen3rdX = 26 / 32;
var _user$project$PyramidView$queen2ndY = 17 / 16;
var _user$project$PyramidView$queen2ndX = 21 / 32;
var _user$project$PyramidView$middleY = 9 / 10;
var _user$project$PyramidView$dotAttributes = function (dotScale) {
	return _elm_lang$core$Native_List.fromArray(
		[
			_elm_lang$svg$Svg_Attributes$stroke('#EEEEEE'),
			_elm_lang$svg$Svg_Attributes$fillOpacity('0.0'),
			_elm_lang$svg$Svg_Attributes$pointerEvents('none'),
			_elm_lang$svg$Svg_Attributes$rx(
			_elm_lang$core$Basics$toString(dotScale / 9)),
			_elm_lang$svg$Svg_Attributes$ry(
			_elm_lang$core$Basics$toString(dotScale / 7))
		]);
};
var _user$project$PyramidView$dot = F2(
	function (dotScale, attributes) {
		return A2(
			_elm_lang$svg$Svg$ellipse,
			A2(
				_elm_lang$core$Basics_ops['++'],
				attributes,
				_user$project$PyramidView$dotAttributes(dotScale)),
			_elm_lang$core$Native_List.fromArray(
				[]));
	});
var _user$project$PyramidView$getScale = function (size) {
	var _p0 = size;
	switch (_p0.ctor) {
		case 'Queen':
			return 160 / 3;
		case 'Drone':
			return 40;
		default:
			return 30;
	}
};
var _user$project$PyramidView$getDots = F3(
	function (size, x, y) {
		var pawnScale = _user$project$PyramidView$getScale(_user$project$Model$Pawn);
		var scale = _user$project$PyramidView$getScale(size);
		var leftMiddleDot = A2(
			_user$project$PyramidView$dot,
			pawnScale,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$svg$Svg_Attributes$cx(
					_elm_lang$core$Basics$toString(x + ((0 - scale) / 2))),
					_elm_lang$svg$Svg_Attributes$cy(
					_elm_lang$core$Basics$toString(y + (0 - (scale * _user$project$PyramidView$middleY))))
				]));
		var rightMiddleDot = A2(
			_user$project$PyramidView$dot,
			pawnScale,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$svg$Svg_Attributes$cx(
					_elm_lang$core$Basics$toString(x + (scale / 2))),
					_elm_lang$svg$Svg_Attributes$cy(
					_elm_lang$core$Basics$toString(y + (0 - (scale * _user$project$PyramidView$middleY))))
				]));
		var _p1 = size;
		switch (_p1.ctor) {
			case 'Queen':
				return _elm_lang$core$Native_List.fromArray(
					[
						leftMiddleDot,
						A2(
						_user$project$PyramidView$dot,
						(pawnScale * 31) / 32,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$svg$Svg_Attributes$cx(
								_elm_lang$core$Basics$toString(x + ((0 - scale) * _user$project$PyramidView$queen2ndX))),
								_elm_lang$svg$Svg_Attributes$cy(
								_elm_lang$core$Basics$toString(y - (scale * _user$project$PyramidView$queen2ndY)))
							])),
						A2(
						_user$project$PyramidView$dot,
						(pawnScale * 30) / 32,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$svg$Svg_Attributes$cx(
								_elm_lang$core$Basics$toString(x + ((0 - scale) * _user$project$PyramidView$queen3rdX))),
								_elm_lang$svg$Svg_Attributes$cy(
								_elm_lang$core$Basics$toString(y - (scale * _user$project$PyramidView$queen3rdY)))
							])),
						rightMiddleDot,
						A2(
						_user$project$PyramidView$dot,
						(pawnScale * 33) / 32,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$svg$Svg_Attributes$cx(
								_elm_lang$core$Basics$toString(x + (scale * (1 - _user$project$PyramidView$queen2ndX)))),
								_elm_lang$svg$Svg_Attributes$cy(
								_elm_lang$core$Basics$toString(y - (scale * ((2 * _user$project$PyramidView$middleY) - _user$project$PyramidView$queen2ndY))))
							])),
						A2(
						_user$project$PyramidView$dot,
						(pawnScale * 34) / 32,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$svg$Svg_Attributes$cx(
								_elm_lang$core$Basics$toString(x + (scale * (1 - _user$project$PyramidView$queen3rdX)))),
								_elm_lang$svg$Svg_Attributes$cy(
								_elm_lang$core$Basics$toString(y - (scale * ((2 * _user$project$PyramidView$middleY) - _user$project$PyramidView$queen3rdY))))
							]))
					]);
			case 'Drone':
				return _elm_lang$core$Native_List.fromArray(
					[
						leftMiddleDot,
						A2(
						_user$project$PyramidView$dot,
						(pawnScale * 31) / 32,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$svg$Svg_Attributes$cx(
								_elm_lang$core$Basics$toString(x + ((0 - scale) * _user$project$PyramidView$drone2ndX))),
								_elm_lang$svg$Svg_Attributes$cy(
								_elm_lang$core$Basics$toString(y - (scale * _user$project$PyramidView$drone2ndY)))
							])),
						rightMiddleDot,
						A2(
						_user$project$PyramidView$dot,
						(pawnScale * 33) / 32,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$svg$Svg_Attributes$cx(
								_elm_lang$core$Basics$toString(x + (scale * (1 - _user$project$PyramidView$drone2ndX)))),
								_elm_lang$svg$Svg_Attributes$cy(
								_elm_lang$core$Basics$toString(y - (scale * ((2 * _user$project$PyramidView$middleY) - _user$project$PyramidView$drone2ndY))))
							]))
					]);
			default:
				return _elm_lang$core$Native_List.fromArray(
					[leftMiddleDot, rightMiddleDot]);
		}
	});
var _user$project$PyramidView$queenPyramidPathSuffix = _user$project$PyramidView$pyramidPathSuffix(
	_user$project$PyramidView$getScale(_user$project$Model$Queen));
var _user$project$PyramidView$dronePyramidPathSuffix = _user$project$PyramidView$pyramidPathSuffix(
	_user$project$PyramidView$getScale(_user$project$Model$Drone));
var _user$project$PyramidView$pawnPyramidPathSuffix = _user$project$PyramidView$pyramidPathSuffix(
	_user$project$PyramidView$getScale(_user$project$Model$Pawn));
var _user$project$PyramidView$pyramid = F4(
	function (attributes, size, x, y) {
		var dString = A2(
			_elm_lang$core$Basics_ops['++'],
			'M ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(x),
				A2(
					_elm_lang$core$Basics_ops['++'],
					' ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(y),
						function () {
							var _p2 = size;
							switch (_p2.ctor) {
								case 'Queen':
									return _user$project$PyramidView$queenPyramidPathSuffix;
								case 'Drone':
									return _user$project$PyramidView$dronePyramidPathSuffix;
								default:
									return _user$project$PyramidView$pawnPyramidPathSuffix;
							}
						}()))));
		return A2(
			_elm_lang$svg$Svg$g,
			_elm_lang$core$Native_List.fromArray(
				[]),
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Native_List.fromArray(
					[
						A2(
						_elm_lang$svg$Svg$path,
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Native_List.fromArray(
								[
									_elm_lang$svg$Svg_Attributes$d(dString),
									_elm_lang$svg$Svg_Attributes$strokeWidth('2'),
									_elm_lang$svg$Svg_Attributes$fillOpacity('0.4')
								]),
							attributes),
						_elm_lang$core$Native_List.fromArray(
							[]))
					]),
				A3(_user$project$PyramidView$getDots, size, x, y)));
	});
var _user$project$PyramidView$clickablePyramid = function (msg) {
	return _user$project$PyramidView$pyramid(
		_elm_lang$core$Native_List.fromArray(
			[
				_elm_lang$svg$Svg_Events$onClick(msg),
				_elm_lang$svg$Svg_Attributes$stroke('grey')
			]));
};
var _user$project$PyramidView$highlightedPyramid = _user$project$PyramidView$pyramid(
	_elm_lang$core$Native_List.fromArray(
		[
			_elm_lang$svg$Svg_Attributes$stroke('white')
		]));
var _user$project$PyramidView$placedPyramid = _user$project$PyramidView$pyramid(
	_elm_lang$core$Native_List.fromArray(
		[
			_elm_lang$svg$Svg_Attributes$stroke('grey'),
			_elm_lang$svg$Svg_Attributes$pointerEvents('none')
		]));
var _user$project$PyramidView$above = F3(
	function (upperSize, lowerSize, y) {
		if (_elm_lang$core$Native_Utils.eq(lowerSize, upperSize)) {
			return y - ((_user$project$PyramidView$getScale(upperSize) * 4) / 5);
		} else {
			var _p3 = {ctor: '_Tuple2', _0: upperSize, _1: lowerSize};
			if (((_p3.ctor === '_Tuple2') && (_p3._0.ctor === 'Pawn')) && (_p3._1.ctor === 'Queen')) {
				return y - ((_user$project$PyramidView$getScale(_user$project$Model$Pawn) * 14) / 5);
			} else {
				return y - ((_user$project$PyramidView$getScale(upperSize) * 7) / 5);
			}
		}
	});
var _user$project$PyramidView$below = F3(
	function (lowerSize, upperSize, y) {
		if (_elm_lang$core$Native_Utils.eq(lowerSize, upperSize)) {
			return y;
		} else {
			var _p4 = {ctor: '_Tuple2', _0: lowerSize, _1: upperSize};
			if (((_p4.ctor === '_Tuple2') && (_p4._0.ctor === 'Pawn')) && (_p4._1.ctor === 'Queen')) {
				return y - ((_user$project$PyramidView$getScale(upperSize) * 2) / 5);
			} else {
				return y - ((_user$project$PyramidView$getScale(upperSize) * 1) / 5);
			}
		}
	});
var _user$project$PyramidView$renderStack = F2(
	function (stack, _p5) {
		var _p6 = _p5;
		var _p9 = _p6._1;
		var _p8 = _p6._0;
		var _p7 = stack;
		switch (_p7.ctor) {
			case 'EmptyStack':
				return _user$project$PyramidView$nullSvg;
			case 'Single':
				return A3(_user$project$PyramidView$placedPyramid, _p7._0, _p8, _p9);
			case 'FullTree':
				return A2(
					_elm_lang$svg$Svg$g,
					_elm_lang$core$Native_List.fromArray(
						[]),
					_elm_lang$core$Native_List.fromArray(
						[
							A3(_user$project$PyramidView$placedPyramid, _user$project$Model$Queen, _p8, _p9),
							A3(
							_user$project$PyramidView$placedPyramid,
							_user$project$Model$Drone,
							_p8,
							A3(_user$project$PyramidView$above, _user$project$Model$Drone, _user$project$Model$Queen, _p9)),
							A3(
							_user$project$PyramidView$placedPyramid,
							_user$project$Model$Pawn,
							_p8,
							A3(
								_user$project$PyramidView$above,
								_user$project$Model$Pawn,
								_user$project$Model$Drone,
								A3(_user$project$PyramidView$above, _user$project$Model$Drone, _user$project$Model$Queen, _p9)))
						]));
			case 'PartialTree':
				return A2(
					_elm_lang$svg$Svg$g,
					_elm_lang$core$Native_List.fromArray(
						[]),
					_elm_lang$core$Native_List.fromArray(
						[
							A3(_user$project$PyramidView$placedPyramid, _user$project$Model$Queen, _p8, _p9),
							A3(
							_user$project$PyramidView$placedPyramid,
							_user$project$Model$Drone,
							_p8,
							A3(_user$project$PyramidView$above, _user$project$Model$Drone, _user$project$Model$Queen, _p9))
						]));
			case 'DroneTree':
				return A2(
					_elm_lang$svg$Svg$g,
					_elm_lang$core$Native_List.fromArray(
						[]),
					_elm_lang$core$Native_List.fromArray(
						[
							A3(_user$project$PyramidView$placedPyramid, _user$project$Model$Drone, _p8, _p9),
							A3(
							_user$project$PyramidView$placedPyramid,
							_user$project$Model$Pawn,
							_p8,
							A3(_user$project$PyramidView$above, _user$project$Model$Pawn, _user$project$Model$Drone, _p9))
						]));
			case 'NoDroneTree':
				return A2(
					_elm_lang$svg$Svg$g,
					_elm_lang$core$Native_List.fromArray(
						[]),
					_elm_lang$core$Native_List.fromArray(
						[
							A3(_user$project$PyramidView$placedPyramid, _user$project$Model$Queen, _p8, _p9),
							A3(
							_user$project$PyramidView$placedPyramid,
							_user$project$Model$Pawn,
							_p8,
							A3(_user$project$PyramidView$above, _user$project$Model$Pawn, _user$project$Model$Queen, _p9))
						]));
			case 'FullNest':
				return A2(
					_elm_lang$svg$Svg$g,
					_elm_lang$core$Native_List.fromArray(
						[]),
					_elm_lang$core$Native_List.fromArray(
						[
							A3(
							_user$project$PyramidView$placedPyramid,
							_user$project$Model$Pawn,
							_p8,
							A3(
								_user$project$PyramidView$below,
								_user$project$Model$Pawn,
								_user$project$Model$Drone,
								A3(_user$project$PyramidView$below, _user$project$Model$Drone, _user$project$Model$Queen, _p9))),
							A3(
							_user$project$PyramidView$placedPyramid,
							_user$project$Model$Drone,
							_p8,
							A3(_user$project$PyramidView$below, _user$project$Model$Drone, _user$project$Model$Queen, _p9)),
							A3(_user$project$PyramidView$placedPyramid, _user$project$Model$Queen, _p8, _p9)
						]));
			case 'PartialNest':
				return A2(
					_elm_lang$svg$Svg$g,
					_elm_lang$core$Native_List.fromArray(
						[]),
					_elm_lang$core$Native_List.fromArray(
						[
							A3(
							_user$project$PyramidView$placedPyramid,
							_user$project$Model$Pawn,
							_p8,
							A3(_user$project$PyramidView$below, _user$project$Model$Pawn, _user$project$Model$Drone, _p9)),
							A3(_user$project$PyramidView$placedPyramid, _user$project$Model$Drone, _p8, _p9)
						]));
			case 'DroneNest':
				return A2(
					_elm_lang$svg$Svg$g,
					_elm_lang$core$Native_List.fromArray(
						[]),
					_elm_lang$core$Native_List.fromArray(
						[
							A3(
							_user$project$PyramidView$placedPyramid,
							_user$project$Model$Drone,
							_p8,
							A3(_user$project$PyramidView$below, _user$project$Model$Drone, _user$project$Model$Queen, _p9)),
							A3(_user$project$PyramidView$placedPyramid, _user$project$Model$Queen, _p8, _p9)
						]));
			default:
				return A2(
					_elm_lang$svg$Svg$g,
					_elm_lang$core$Native_List.fromArray(
						[]),
					_elm_lang$core$Native_List.fromArray(
						[
							A3(
							_user$project$PyramidView$placedPyramid,
							_user$project$Model$Pawn,
							_p8,
							A3(_user$project$PyramidView$below, _user$project$Model$Pawn, _user$project$Model$Queen, _p9)),
							A3(_user$project$PyramidView$placedPyramid, _user$project$Model$Queen, _p8, _p9)
						]));
		}
	});
var _user$project$PyramidView$stashSpacing = 5;
var _user$project$PyramidView$stashHeight = A2(
	F2(
		function (x, y) {
			return x + y;
		}),
	2 * _user$project$PyramidView$stashSpacing,
	A2(
		F2(
			function (x, y) {
				return x + y;
			}),
		_user$project$PyramidView$pyramidHeightConstant * _user$project$PyramidView$getScale(_user$project$Model$Queen),
		A2(
			F2(
				function (x, y) {
					return x * y;
				}),
			_user$project$Model$maxStashAmount - 1,
			_elm_lang$core$Basics$abs(
				A3(_user$project$PyramidView$above, _user$project$Model$Queen, _user$project$Model$Queen, 0)))));
var _user$project$PyramidView$stashHeightString = _elm_lang$core$Basics$toString(_user$project$PyramidView$stashHeight);
var _user$project$PyramidView$getHeights = F2(
	function (amount, size) {
		return A3(
			_elm_lang$core$List$scanl,
			_user$project$PyramidView$above(size),
			_user$project$PyramidView$stashHeight - _user$project$PyramidView$stashSpacing,
			A2(_elm_lang$core$List$repeat, amount, size));
	});
var _user$project$PyramidView$renderStashStack = F4(
	function (selected, x, amount, size) {
		var pyramidView = function () {
			var _p10 = selected;
			if (_p10.ctor === 'Just') {
				return _elm_lang$core$Native_Utils.eq(size, _p10._0) ? _user$project$PyramidView$highlightedPyramid : _user$project$PyramidView$clickablePyramid(
					_user$project$Msg$Select(
						_elm_lang$core$Maybe$Just(size)));
			} else {
				return _user$project$PyramidView$clickablePyramid(
					_user$project$Msg$Select(
						_elm_lang$core$Maybe$Just(size)));
			}
		}();
		return A2(
			_elm_lang$core$List$map,
			A2(pyramidView, size, x),
			_elm_lang$core$List$reverse(
				A2(
					_elm_lang$core$Maybe$withDefault,
					_elm_lang$core$Native_List.fromArray(
						[]),
					_elm_lang$core$List$tail(
						_elm_lang$core$List$reverse(
							A2(_user$project$PyramidView$getHeights, amount, size))))));
	});
var _user$project$PyramidView$stashWidth = 500;
var _user$project$PyramidView$stashWidthString = _elm_lang$core$Basics$toString(_user$project$PyramidView$stashWidth);
var _user$project$PyramidView$renderStash = F2(
	function (selected, stash) {
		return A2(
			_elm_lang$svg$Svg$svg,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$svg$Svg_Attributes$width(_user$project$PyramidView$stashWidthString),
					_elm_lang$svg$Svg_Attributes$height(_user$project$PyramidView$stashHeightString),
					_elm_lang$svg$Svg_Attributes$viewBox(
					A2(
						_elm_lang$core$Basics_ops['++'],
						'0 0 ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_user$project$PyramidView$stashWidthString,
							A2(_elm_lang$core$Basics_ops['++'], ' ', _user$project$PyramidView$stashHeightString))))
				]),
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Native_List.fromArray(
					[
						A2(
						_elm_lang$svg$Svg$rect,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$svg$Svg_Attributes$x('0'),
								_elm_lang$svg$Svg_Attributes$y('0'),
								_elm_lang$svg$Svg_Attributes$width(_user$project$PyramidView$stashWidthString),
								_elm_lang$svg$Svg_Attributes$height(_user$project$PyramidView$stashHeightString),
								_elm_lang$svg$Svg_Attributes$stroke('black'),
								_elm_lang$svg$Svg_Attributes$strokeWidth('2'),
								_elm_lang$svg$Svg_Attributes$fillOpacity('0')
							]),
						_elm_lang$core$Native_List.fromArray(
							[]))
					]),
				A2(
					_elm_lang$core$Basics_ops['++'],
					A4(_user$project$PyramidView$renderStashStack, selected, (_user$project$PyramidView$stashWidth * 1) / 6, stash.queen, _user$project$Model$Queen),
					A2(
						_elm_lang$core$Basics_ops['++'],
						A4(_user$project$PyramidView$renderStashStack, selected, (_user$project$PyramidView$stashWidth * 3) / 6, stash.drone, _user$project$Model$Drone),
						A4(_user$project$PyramidView$renderStashStack, selected, (_user$project$PyramidView$stashWidth * 5) / 6, stash.pawn, _user$project$Model$Pawn)))));
	});

var _user$project$View$queenScale = _user$project$PyramidView$getScale(_user$project$Model$Queen);
var _user$project$View$queenScaleString = _elm_lang$core$Basics$toString(_user$project$View$queenScale);
var _user$project$View$square = function (x) {
	return Math.pow(x, 2);
};
var _user$project$View$spaceSideLength = (_elm_lang$core$Basics$sqrt(
	2.5 * _user$project$View$square(_user$project$View$queenScale)) * 7) / 5;
var _user$project$View$spaceOffset = _elm_lang$core$Basics$sqrt(
	_user$project$View$square(_user$project$View$spaceSideLength) / 2);
var _user$project$View$halfSpaceOffset = _user$project$View$spaceOffset / 2;
var _user$project$View$threeHalfsSpaceOffset = _user$project$View$halfSpaceOffset * 3;
var _user$project$View$fiveHalfsSpaceOffset = _user$project$View$halfSpaceOffset * 5;
var _user$project$View$suffixSpaceOffset = _user$project$View$spaceOffset - 2.5;
var _user$project$View$suffixSpaceOffsetString = _elm_lang$core$Basics$toString(_user$project$View$suffixSpaceOffset);
var _user$project$View$minusSuffixSpaceOffsetString = _elm_lang$core$Basics$toString(0 - _user$project$View$suffixSpaceOffset);
var _user$project$View$spaceSuffix = A2(
	_elm_lang$core$Basics_ops['++'],
	A2(
		_elm_lang$core$Basics_ops['++'],
		' m 0 ',
		_elm_lang$core$Basics$toString(_user$project$View$queenScale / 2)),
	A2(
		_elm_lang$core$Basics_ops['++'],
		A2(
			_elm_lang$core$Basics_ops['++'],
			' l ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_user$project$View$suffixSpaceOffsetString,
				A2(_elm_lang$core$Basics_ops['++'], ' ', _user$project$View$minusSuffixSpaceOffsetString))),
		A2(
			_elm_lang$core$Basics_ops['++'],
			A2(
				_elm_lang$core$Basics_ops['++'],
				' l ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$View$minusSuffixSpaceOffsetString,
					A2(_elm_lang$core$Basics_ops['++'], ' ', _user$project$View$minusSuffixSpaceOffsetString))),
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(
					_elm_lang$core$Basics_ops['++'],
					' l ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_user$project$View$minusSuffixSpaceOffsetString,
						A2(_elm_lang$core$Basics_ops['++'], ' ', _user$project$View$suffixSpaceOffsetString))),
				'Z'))));
var _user$project$View$doubleSpaceOffset = _user$project$View$spaceOffset * 2;
var _user$project$View$edgeSpace = F3(
	function (_p0, edgeId, selected) {
		var _p1 = _p0;
		var msgAttributes = function () {
			var _p2 = selected;
			if (_p2.ctor === 'Just') {
				return _elm_lang$core$Native_List.fromArray(
					[
						_elm_lang$svg$Svg_Attributes$stroke('white'),
						_elm_lang$svg$Svg_Events$onClick(
						_user$project$Msg$PlaceOnEdge(edgeId))
					]);
			} else {
				return _elm_lang$core$Native_List.fromArray(
					[
						_elm_lang$svg$Svg_Attributes$stroke('black')
					]);
			}
		}();
		var dString = A2(
			_elm_lang$core$Basics_ops['++'],
			A2(
				_elm_lang$core$Basics_ops['++'],
				'M ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(_p1._0),
					A2(
						_elm_lang$core$Basics_ops['++'],
						' ',
						_elm_lang$core$Basics$toString(_p1._1)))),
			_user$project$View$spaceSuffix);
		return A2(
			_elm_lang$svg$Svg$g,
			_elm_lang$core$Native_List.fromArray(
				[]),
			_elm_lang$core$Native_List.fromArray(
				[
					A2(
					_elm_lang$svg$Svg$path,
					A2(
						_elm_lang$core$Basics_ops['++'],
						msgAttributes,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$svg$Svg_Attributes$d(dString),
								_elm_lang$svg$Svg_Attributes$fill('#444444'),
								_elm_lang$svg$Svg_Attributes$fillOpacity('0.4'),
								_elm_lang$svg$Svg_Attributes$strokeWidth('4')
							])),
					_elm_lang$core$Native_List.fromArray(
						[]))
				]));
	});
var _user$project$View$space = F2(
	function (_p3, maybeMsg) {
		var _p4 = _p3;
		var msgAttributes = function () {
			var _p5 = maybeMsg;
			if (_p5.ctor === 'Just') {
				return _elm_lang$core$Native_List.fromArray(
					[
						_elm_lang$svg$Svg_Attributes$stroke('white'),
						_elm_lang$svg$Svg_Events$onClick(_p5._0)
					]);
			} else {
				return _elm_lang$core$Native_List.fromArray(
					[
						_elm_lang$svg$Svg_Attributes$stroke('black')
					]);
			}
		}();
		var dString = A2(
			_elm_lang$core$Basics_ops['++'],
			A2(
				_elm_lang$core$Basics_ops['++'],
				'M ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(_p4._0),
					A2(
						_elm_lang$core$Basics_ops['++'],
						' ',
						_elm_lang$core$Basics$toString(_p4._1)))),
			_user$project$View$spaceSuffix);
		return A2(
			_elm_lang$svg$Svg$g,
			_elm_lang$core$Native_List.fromArray(
				[]),
			_elm_lang$core$Native_List.fromArray(
				[
					A2(
					_elm_lang$svg$Svg$path,
					A2(
						_elm_lang$core$Basics_ops['++'],
						msgAttributes,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$svg$Svg_Attributes$d(dString),
								_elm_lang$svg$Svg_Attributes$fill('#444444'),
								_elm_lang$svg$Svg_Attributes$strokeWidth('4')
							])),
					_elm_lang$core$Native_List.fromArray(
						[]))
				]));
	});
var _user$project$View$spaceMsg = F3(
	function (boardId, selected, stack) {
		var _p6 = selected;
		if (_p6.ctor === 'Just') {
			return A2(_user$project$Model$sizeFits, _p6._0, stack) ? _elm_lang$core$Maybe$Just(
				_user$project$Msg$Place(boardId)) : _elm_lang$core$Maybe$Nothing;
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _user$project$View$spaceAndStack = F4(
	function (boardId, selected, stack, coords) {
		return _elm_lang$core$Native_List.fromArray(
			[
				A2(
				_user$project$View$space,
				coords,
				A3(_user$project$View$spaceMsg, boardId, selected, stack)),
				A2(_user$project$PyramidView$renderStack, stack, coords)
			]);
	});
var _user$project$View$tupleAdd = F2(
	function (_p8, _p7) {
		var _p9 = _p8;
		var _p10 = _p7;
		return {ctor: '_Tuple2', _0: _p9._0 + _p10._0, _1: _p9._1 + _p10._1};
	});
var _user$project$View$boardHeight = 720;
var _user$project$View$boardHeightString = _elm_lang$core$Basics$toString(_user$project$View$boardHeight);
var _user$project$View$centerY = _user$project$View$boardHeight / 2;
var _user$project$View$boardOffsetY = _user$project$View$centerY - _user$project$View$halfSpaceOffset;
var _user$project$View$boardWidth = 720;
var _user$project$View$boardWidthString = _elm_lang$core$Basics$toString(_user$project$View$boardWidth);
var _user$project$View$centerX = _user$project$View$boardWidth / 2;
var _user$project$View$boardOffsetX = _user$project$View$centerX;
var _user$project$View$atBoardOffset = {ctor: '_Tuple2', _0: _user$project$View$boardOffsetX, _1: _user$project$View$boardOffsetY};
var _user$project$View$fromBoardOffset = F2(
	function (x, y) {
		return {ctor: '_Tuple2', _0: _user$project$View$boardOffsetX + x, _1: _user$project$View$boardOffsetY + y};
	});
var _user$project$View$edgeSpaces = F2(
	function (selected, board) {
		var _p11 = board;
		switch (_p11.ctor) {
			case 'EmptyBoard':
				return _elm_lang$core$Native_List.fromArray(
					[]);
			case 'OneByOne':
				return _elm_lang$core$Native_List.fromArray(
					[
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0, 0 - _user$project$View$spaceOffset),
						_user$project$Model$EdgeZeroZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$spaceOffset, 0),
						_user$project$Model$EdgeOneZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$doubleSpaceOffset, _user$project$View$spaceOffset),
						_user$project$Model$EdgeTwoZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$spaceOffset, 0),
						_user$project$Model$EdgeZeroOne,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$spaceOffset, _user$project$View$doubleSpaceOffset),
						_user$project$Model$EdgeTwoOne,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$doubleSpaceOffset, _user$project$View$spaceOffset),
						_user$project$Model$EdgeZeroTwo,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$spaceOffset, _user$project$View$doubleSpaceOffset),
						_user$project$Model$EdgeOneTwo,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0, 3 * _user$project$View$spaceOffset),
						_user$project$Model$EdgeTwoTwo,
						selected)
					]);
			case 'OneByTwo':
				return _elm_lang$core$Native_List.fromArray(
					[
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$halfSpaceOffset, 0 - _user$project$View$threeHalfsSpaceOffset),
						_user$project$Model$EdgeZeroZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$threeHalfsSpaceOffset, 0 - _user$project$View$halfSpaceOffset),
						_user$project$Model$EdgeOneZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$fiveHalfsSpaceOffset, _user$project$View$halfSpaceOffset),
						_user$project$Model$EdgeTwoZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$halfSpaceOffset, 0 - _user$project$View$halfSpaceOffset),
						_user$project$Model$EdgeZeroOne,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$threeHalfsSpaceOffset, _user$project$View$threeHalfsSpaceOffset),
						_user$project$Model$EdgeTwoOne,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$threeHalfsSpaceOffset, _user$project$View$halfSpaceOffset),
						_user$project$Model$EdgeZeroTwo,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$halfSpaceOffset, _user$project$View$fiveHalfsSpaceOffset),
						_user$project$Model$EdgeTwoTwo,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$fiveHalfsSpaceOffset, _user$project$View$threeHalfsSpaceOffset),
						_user$project$Model$EdgeZeroThree,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$threeHalfsSpaceOffset, _user$project$View$fiveHalfsSpaceOffset),
						_user$project$Model$EdgeOneThree,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$halfSpaceOffset, 3.5 * _user$project$View$spaceOffset),
						_user$project$Model$EdgeTwoThree,
						selected)
					]);
			case 'TwoByOne':
				return _elm_lang$core$Native_List.fromArray(
					[
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$halfSpaceOffset, 0 - _user$project$View$threeHalfsSpaceOffset),
						_user$project$Model$EdgeZeroZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$halfSpaceOffset, 0 - _user$project$View$halfSpaceOffset),
						_user$project$Model$EdgeOneZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$threeHalfsSpaceOffset, _user$project$View$halfSpaceOffset),
						_user$project$Model$EdgeTwoZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$fiveHalfsSpaceOffset, _user$project$View$threeHalfsSpaceOffset),
						_user$project$Model$EdgeThreeZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$threeHalfsSpaceOffset, 0 - _user$project$View$halfSpaceOffset),
						_user$project$Model$EdgeZeroOne,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$threeHalfsSpaceOffset, _user$project$View$fiveHalfsSpaceOffset),
						_user$project$Model$EdgeThreeOne,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$fiveHalfsSpaceOffset, _user$project$View$halfSpaceOffset),
						_user$project$Model$EdgeZeroTwo,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$threeHalfsSpaceOffset, _user$project$View$threeHalfsSpaceOffset),
						_user$project$Model$EdgeOneTwo,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$halfSpaceOffset, _user$project$View$fiveHalfsSpaceOffset),
						_user$project$Model$EdgeTwoTwo,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$halfSpaceOffset, 3.5 * _user$project$View$spaceOffset),
						_user$project$Model$EdgeThreeTwo,
						selected)
					]);
			case 'OneByThree':
				return _elm_lang$core$Native_List.fromArray(
					[
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$halfSpaceOffset, 0 - _user$project$View$spaceOffset),
						_user$project$Model$EdgeZeroZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$threeHalfsSpaceOffset, _user$project$View$spaceOffset),
						_user$project$Model$EdgeTwoZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$threeHalfsSpaceOffset, 0),
						_user$project$Model$EdgeZeroOne,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$halfSpaceOffset, _user$project$View$doubleSpaceOffset),
						_user$project$Model$EdgeTwoOne,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$fiveHalfsSpaceOffset, _user$project$View$spaceOffset),
						_user$project$Model$EdgeZeroTwo,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$halfSpaceOffset, 3 * _user$project$View$spaceOffset),
						_user$project$Model$EdgeTwoTwo,
						selected)
					]);
			case 'ThreeByOne':
				return _elm_lang$core$Native_List.fromArray(
					[
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$halfSpaceOffset, 0 - _user$project$View$halfSpaceOffset),
						_user$project$Model$EdgeZeroZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$threeHalfsSpaceOffset, _user$project$View$halfSpaceOffset),
						_user$project$Model$EdgeOneZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$fiveHalfsSpaceOffset, _user$project$View$threeHalfsSpaceOffset),
						_user$project$Model$EdgeTwoZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$threeHalfsSpaceOffset, _user$project$View$threeHalfsSpaceOffset),
						_user$project$Model$EdgeZeroTwo,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$halfSpaceOffset, _user$project$View$fiveHalfsSpaceOffset),
						_user$project$Model$EdgeOneTwo,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$halfSpaceOffset, 3.5 * _user$project$View$spaceOffset),
						_user$project$Model$EdgeTwoTwo,
						selected)
					]);
			case 'TwoByTwo':
				return _elm_lang$core$Native_List.fromArray(
					[
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0, 0 - _user$project$View$doubleSpaceOffset),
						_user$project$Model$EdgeZeroZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$spaceOffset, 0 - _user$project$View$spaceOffset),
						_user$project$Model$EdgeOneZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$doubleSpaceOffset, 0),
						_user$project$Model$EdgeTwoZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 3 * _user$project$View$spaceOffset, _user$project$View$spaceOffset),
						_user$project$Model$EdgeThreeZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$spaceOffset, 0 - _user$project$View$spaceOffset),
						_user$project$Model$EdgeZeroOne,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$doubleSpaceOffset, _user$project$View$doubleSpaceOffset),
						_user$project$Model$EdgeThreeOne,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$doubleSpaceOffset, 0),
						_user$project$Model$EdgeZeroTwo,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$spaceOffset, 3 * _user$project$View$spaceOffset),
						_user$project$Model$EdgeThreeTwo,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - (3 * _user$project$View$spaceOffset), _user$project$View$spaceOffset),
						_user$project$Model$EdgeZeroThree,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$doubleSpaceOffset, _user$project$View$doubleSpaceOffset),
						_user$project$Model$EdgeOneThree,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$spaceOffset, 3 * _user$project$View$spaceOffset),
						_user$project$Model$EdgeTwoThree,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0, 4 * _user$project$View$spaceOffset),
						_user$project$Model$EdgeThreeThree,
						selected)
					]);
			case 'TwoByThree':
				return _elm_lang$core$Native_List.fromArray(
					[
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$spaceOffset, 0 - _user$project$View$threeHalfsSpaceOffset),
						_user$project$Model$EdgeZeroZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$doubleSpaceOffset, _user$project$View$threeHalfsSpaceOffset),
						_user$project$Model$EdgeThreeZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$doubleSpaceOffset, 0 - _user$project$View$halfSpaceOffset),
						_user$project$Model$EdgeZeroOne,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$spaceOffset, _user$project$View$fiveHalfsSpaceOffset),
						_user$project$Model$EdgeThreeOne,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - (3 * _user$project$View$spaceOffset), _user$project$View$halfSpaceOffset),
						_user$project$Model$EdgeZeroTwo,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0, 3.5 * _user$project$View$spaceOffset),
						_user$project$Model$EdgeThreeTwo,
						selected)
					]);
			case 'ThreeByTwo':
				return _elm_lang$core$Native_List.fromArray(
					[
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$halfSpaceOffset, 0 - _user$project$View$threeHalfsSpaceOffset),
						_user$project$Model$EdgeZeroZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$threeHalfsSpaceOffset, 0 - _user$project$View$halfSpaceOffset),
						_user$project$Model$EdgeOneZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, _user$project$View$fiveHalfsSpaceOffset, _user$project$View$halfSpaceOffset),
						_user$project$Model$EdgeTwoZero,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$fiveHalfsSpaceOffset, _user$project$View$threeHalfsSpaceOffset),
						_user$project$Model$EdgeZeroThree,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$threeHalfsSpaceOffset, _user$project$View$fiveHalfsSpaceOffset),
						_user$project$Model$EdgeOneThree,
						selected),
						A3(
						_user$project$View$edgeSpace,
						A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$halfSpaceOffset, 3.5 * _user$project$View$spaceOffset),
						_user$project$Model$EdgeTwoThree,
						selected)
					]);
			default:
				return _elm_lang$core$Native_List.fromArray(
					[]);
		}
	});
var _user$project$View$renderBoard = F2(
	function (selected, board) {
		var determinedSpace = function () {
			var _p12 = board;
			switch (_p12.ctor) {
				case 'EmptyBoard':
					return A4(
						_user$project$View$spaceAndStack,
						_user$project$Model$ZeroZero,
						selected,
						_user$project$Model$EmptyStack,
						A2(_user$project$View$fromBoardOffset, 0, _user$project$View$spaceOffset));
				case 'OneByOne':
					return A4(
						_user$project$View$spaceAndStack,
						_user$project$Model$ZeroZero,
						selected,
						_p12._0,
						A2(_user$project$View$fromBoardOffset, 0, _user$project$View$spaceOffset));
				case 'OneByTwo':
					return A2(
						_elm_lang$core$Basics_ops['++'],
						A4(
							_user$project$View$spaceAndStack,
							_user$project$Model$ZeroZero,
							selected,
							_p12._0,
							A2(_user$project$View$fromBoardOffset, _user$project$View$halfSpaceOffset, _user$project$View$halfSpaceOffset)),
						A4(
							_user$project$View$spaceAndStack,
							_user$project$Model$ZeroOne,
							selected,
							_p12._1,
							A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$halfSpaceOffset, _user$project$View$threeHalfsSpaceOffset)));
				case 'TwoByOne':
					return A2(
						_elm_lang$core$Basics_ops['++'],
						A4(
							_user$project$View$spaceAndStack,
							_user$project$Model$ZeroZero,
							selected,
							_p12._0,
							A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$halfSpaceOffset, _user$project$View$halfSpaceOffset)),
						A4(
							_user$project$View$spaceAndStack,
							_user$project$Model$OneZero,
							selected,
							_p12._1,
							A2(_user$project$View$fromBoardOffset, _user$project$View$halfSpaceOffset, _user$project$View$threeHalfsSpaceOffset)));
				case 'OneByThree':
					return A2(
						_elm_lang$core$Basics_ops['++'],
						A4(
							_user$project$View$spaceAndStack,
							_user$project$Model$ZeroZero,
							selected,
							_p12._0,
							A2(_user$project$View$fromBoardOffset, _user$project$View$halfSpaceOffset, 0)),
						A2(
							_elm_lang$core$Basics_ops['++'],
							A4(
								_user$project$View$spaceAndStack,
								_user$project$Model$OneZero,
								selected,
								_p12._1,
								A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$halfSpaceOffset, _user$project$View$spaceOffset)),
							A4(
								_user$project$View$spaceAndStack,
								_user$project$Model$TwoZero,
								selected,
								_p12._2,
								A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$threeHalfsSpaceOffset, _user$project$View$doubleSpaceOffset))));
				case 'ThreeByOne':
					return A2(
						_elm_lang$core$Basics_ops['++'],
						A4(
							_user$project$View$spaceAndStack,
							_user$project$Model$ZeroZero,
							selected,
							_p12._0,
							A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$halfSpaceOffset, _user$project$View$halfSpaceOffset)),
						A2(
							_elm_lang$core$Basics_ops['++'],
							A4(
								_user$project$View$spaceAndStack,
								_user$project$Model$ZeroOne,
								selected,
								_p12._1,
								A2(_user$project$View$fromBoardOffset, _user$project$View$halfSpaceOffset, _user$project$View$threeHalfsSpaceOffset)),
							A4(
								_user$project$View$spaceAndStack,
								_user$project$Model$ZeroTwo,
								selected,
								_p12._2,
								A2(_user$project$View$fromBoardOffset, _user$project$View$threeHalfsSpaceOffset, _user$project$View$fiveHalfsSpaceOffset))));
				case 'TwoByTwo':
					var _p13 = _p12._0;
					return A2(
						_elm_lang$core$Basics_ops['++'],
						A4(_user$project$View$spaceAndStack, _user$project$Model$ZeroZero, selected, _p13.zeroZero, _user$project$View$atBoardOffset),
						A2(
							_elm_lang$core$Basics_ops['++'],
							A4(
								_user$project$View$spaceAndStack,
								_user$project$Model$OneZero,
								selected,
								_p13.oneZero,
								A2(_user$project$View$fromBoardOffset, _user$project$View$spaceOffset, _user$project$View$spaceOffset)),
							A2(
								_elm_lang$core$Basics_ops['++'],
								A4(
									_user$project$View$spaceAndStack,
									_user$project$Model$ZeroOne,
									selected,
									_p13.zeroOne,
									A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$spaceOffset, _user$project$View$spaceOffset)),
								A4(
									_user$project$View$spaceAndStack,
									_user$project$Model$OneOne,
									selected,
									_p13.oneOne,
									A2(_user$project$View$fromBoardOffset, 0, _user$project$View$doubleSpaceOffset)))));
				case 'TwoByThree':
					var _p14 = _p12._0;
					return A2(
						_elm_lang$core$Basics_ops['++'],
						A4(
							_user$project$View$spaceAndStack,
							_user$project$Model$ZeroZero,
							selected,
							_p14.zeroZero,
							A2(_user$project$View$fromBoardOffset, 0, 0 - _user$project$View$halfSpaceOffset)),
						A2(
							_elm_lang$core$Basics_ops['++'],
							A4(
								_user$project$View$spaceAndStack,
								_user$project$Model$OneZero,
								selected,
								_p14.oneZero,
								A2(_user$project$View$fromBoardOffset, _user$project$View$spaceOffset, _user$project$View$halfSpaceOffset)),
							A2(
								_elm_lang$core$Basics_ops['++'],
								A4(
									_user$project$View$spaceAndStack,
									_user$project$Model$ZeroOne,
									selected,
									_p14.zeroOne,
									A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$spaceOffset, _user$project$View$halfSpaceOffset)),
								A2(
									_elm_lang$core$Basics_ops['++'],
									A4(
										_user$project$View$spaceAndStack,
										_user$project$Model$OneOne,
										selected,
										_p14.oneOne,
										A2(_user$project$View$fromBoardOffset, 0, _user$project$View$threeHalfsSpaceOffset)),
									A2(
										_elm_lang$core$Basics_ops['++'],
										A4(
											_user$project$View$spaceAndStack,
											_user$project$Model$ZeroTwo,
											selected,
											_p14.zeroTwo,
											A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$doubleSpaceOffset, _user$project$View$threeHalfsSpaceOffset)),
										A4(
											_user$project$View$spaceAndStack,
											_user$project$Model$OneTwo,
											selected,
											_p14.oneTwo,
											A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$spaceOffset, _user$project$View$fiveHalfsSpaceOffset)))))));
				case 'ThreeByTwo':
					var _p15 = _p12._0;
					return A2(
						_elm_lang$core$Basics_ops['++'],
						A4(
							_user$project$View$spaceAndStack,
							_user$project$Model$ZeroZero,
							selected,
							_p15.zeroZero,
							A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$halfSpaceOffset, 0 - _user$project$View$halfSpaceOffset)),
						A2(
							_elm_lang$core$Basics_ops['++'],
							A4(
								_user$project$View$spaceAndStack,
								_user$project$Model$OneZero,
								selected,
								_p15.oneZero,
								A2(_user$project$View$fromBoardOffset, _user$project$View$halfSpaceOffset, _user$project$View$halfSpaceOffset)),
							A2(
								_elm_lang$core$Basics_ops['++'],
								A4(
									_user$project$View$spaceAndStack,
									_user$project$Model$TwoZero,
									selected,
									_p15.twoZero,
									A2(_user$project$View$fromBoardOffset, _user$project$View$threeHalfsSpaceOffset, _user$project$View$threeHalfsSpaceOffset)),
								A2(
									_elm_lang$core$Basics_ops['++'],
									A4(
										_user$project$View$spaceAndStack,
										_user$project$Model$ZeroOne,
										selected,
										_p15.zeroOne,
										A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$threeHalfsSpaceOffset, _user$project$View$halfSpaceOffset)),
									A2(
										_elm_lang$core$Basics_ops['++'],
										A4(
											_user$project$View$spaceAndStack,
											_user$project$Model$OneOne,
											selected,
											_p15.oneOne,
											A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$halfSpaceOffset, _user$project$View$threeHalfsSpaceOffset)),
										A4(
											_user$project$View$spaceAndStack,
											_user$project$Model$TwoOne,
											selected,
											_p15.twoOne,
											A2(_user$project$View$fromBoardOffset, _user$project$View$halfSpaceOffset, _user$project$View$fiveHalfsSpaceOffset)))))));
				default:
					var _p16 = _p12._0;
					return A2(
						_elm_lang$core$Basics_ops['++'],
						A4(
							_user$project$View$spaceAndStack,
							_user$project$Model$ZeroZero,
							selected,
							_p16.zeroZero,
							A2(_user$project$View$fromBoardOffset, 0, 0 - _user$project$View$spaceOffset)),
						A2(
							_elm_lang$core$Basics_ops['++'],
							A4(
								_user$project$View$spaceAndStack,
								_user$project$Model$OneZero,
								selected,
								_p16.oneZero,
								A2(_user$project$View$fromBoardOffset, _user$project$View$spaceOffset, 0)),
							A2(
								_elm_lang$core$Basics_ops['++'],
								A4(
									_user$project$View$spaceAndStack,
									_user$project$Model$TwoZero,
									selected,
									_p16.twoZero,
									A2(_user$project$View$fromBoardOffset, _user$project$View$doubleSpaceOffset, _user$project$View$spaceOffset)),
								A2(
									_elm_lang$core$Basics_ops['++'],
									A4(
										_user$project$View$spaceAndStack,
										_user$project$Model$ZeroOne,
										selected,
										_p16.zeroOne,
										A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$spaceOffset, 0)),
									A2(
										_elm_lang$core$Basics_ops['++'],
										A4(
											_user$project$View$spaceAndStack,
											_user$project$Model$OneOne,
											selected,
											_p16.oneOne,
											A2(_user$project$View$fromBoardOffset, 0, _user$project$View$spaceOffset)),
										A2(
											_elm_lang$core$Basics_ops['++'],
											A4(
												_user$project$View$spaceAndStack,
												_user$project$Model$TwoOne,
												selected,
												_p16.twoOne,
												A2(_user$project$View$fromBoardOffset, _user$project$View$spaceOffset, _user$project$View$doubleSpaceOffset)),
											A2(
												_elm_lang$core$Basics_ops['++'],
												A4(
													_user$project$View$spaceAndStack,
													_user$project$Model$ZeroTwo,
													selected,
													_p16.zeroTwo,
													A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$doubleSpaceOffset, _user$project$View$spaceOffset)),
												A2(
													_elm_lang$core$Basics_ops['++'],
													A4(
														_user$project$View$spaceAndStack,
														_user$project$Model$OneTwo,
														selected,
														_p16.oneTwo,
														A2(_user$project$View$fromBoardOffset, 0 - _user$project$View$spaceOffset, _user$project$View$doubleSpaceOffset)),
													A4(
														_user$project$View$spaceAndStack,
														_user$project$Model$TwoTwo,
														selected,
														_p16.twoTwo,
														A2(_user$project$View$fromBoardOffset, 0, 3 * _user$project$View$spaceOffset))))))))));
			}
		}();
		return A2(
			_elm_lang$svg$Svg$g,
			_elm_lang$core$Native_List.fromArray(
				[]),
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(_user$project$View$edgeSpaces, selected, board),
				determinedSpace));
	});
var _user$project$View$outcomeToString = function (outcome) {
	var _p17 = outcome;
	switch (_p17.ctor) {
		case 'TBD':
			return '';
		case 'UserWin':
			return 'You win!';
		case 'UserWinByExhaustion':
			return 'The CPU has no legal moves, so you win!';
		case 'CPUWin':
			return 'You lost!';
		case 'CPUWinByExhaustion':
			return 'You have no legal moves, so you lost!';
		default:
			return 'No pieces are left so it\'s a tie';
	}
};
var _user$project$View$view = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		_elm_lang$core$Native_List.fromArray(
			[]),
		_elm_lang$core$Native_List.fromArray(
			[
				A2(
				_elm_lang$html$Html$div,
				_elm_lang$core$Native_List.fromArray(
					[
						_elm_lang$html$Html_Attributes$style(
						_elm_lang$core$Native_List.fromArray(
							[
								{ctor: '_Tuple2', _0: 'display', _1: 'flex'},
								{ctor: '_Tuple2', _0: 'justify-content', _1: 'center'}
							]))
					]),
				_elm_lang$core$Native_List.fromArray(
					[
						A5(
						_debois$elm_mdl$Material_Button$render,
						_user$project$Msg$Mdl,
						_elm_lang$core$Native_List.fromArray(
							[0]),
						model.mdl,
						_elm_lang$core$Native_List.fromArray(
							[
								_debois$elm_mdl$Material_Button$raised,
								_debois$elm_mdl$Material_Button$ripple,
								_debois$elm_mdl$Material_Button$onClick(_user$project$Msg$NewGame)
							]),
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$html$Html$text('New Game')
							]))
					])),
				A2(
				_debois$elm_mdl$Material_Grid$grid,
				_elm_lang$core$Native_List.fromArray(
					[]),
				_elm_lang$core$Native_List.fromArray(
					[
						A2(
						_debois$elm_mdl$Material_Grid$cell,
						_elm_lang$core$Native_List.fromArray(
							[
								A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 5)
							]),
						_elm_lang$core$Native_List.fromArray(
							[
								A2(_user$project$PyramidView$renderStash, model.selected, model.stash)
							])),
						A2(
						_debois$elm_mdl$Material_Grid$cell,
						_elm_lang$core$Native_List.fromArray(
							[
								A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 6)
							]),
						_elm_lang$core$Native_List.fromArray(
							[
								A2(
								_elm_lang$html$Html$div,
								_elm_lang$core$Native_List.fromArray(
									[
										_elm_lang$html$Html_Attributes$style(
										_elm_lang$core$Native_List.fromArray(
											[
												{
												ctor: '_Tuple2',
												_0: 'width',
												_1: A2(_elm_lang$core$Basics_ops['++'], _user$project$View$boardWidthString, 'px')
											},
												{ctor: '_Tuple2', _0: 'display', _1: 'flex'},
												{ctor: '_Tuple2', _0: 'justify-content', _1: 'center'},
												{
												ctor: '_Tuple2',
												_0: 'font-size',
												_1: A2(
													_elm_lang$core$Basics_ops['++'],
													_elm_lang$core$Basics$toString(_user$project$View$boardWidth / 16),
													'px')
											}
											]))
									]),
								_elm_lang$core$Native_List.fromArray(
									[
										_elm_lang$html$Html$text(
										_user$project$View$outcomeToString(model.outcome))
									])),
								A2(
								_elm_lang$svg$Svg$svg,
								_elm_lang$core$Native_List.fromArray(
									[
										_elm_lang$svg$Svg_Attributes$width(_user$project$View$boardWidthString),
										_elm_lang$svg$Svg_Attributes$height(_user$project$View$boardHeightString),
										_elm_lang$svg$Svg_Attributes$viewBox(
										A2(
											_elm_lang$core$Basics_ops['++'],
											'0 0 ',
											A2(
												_elm_lang$core$Basics_ops['++'],
												_user$project$View$boardWidthString,
												A2(_elm_lang$core$Basics_ops['++'], ' ', _user$project$View$boardHeightString))))
									]),
								_elm_lang$core$Native_List.fromArray(
									[
										A2(_user$project$View$renderBoard, model.selected, model.board)
									]))
							]))
					]))
			]));
};

var _user$project$Update$shuffle = F2(
	function (seed, list) {
		var length = _elm_lang$core$List$length(list);
		var randomTags = _elm_lang$core$Basics$fst(
			A2(
				_mgold$elm_random_pcg$Random_Pcg$step,
				A2(
					_mgold$elm_random_pcg$Random_Pcg$list,
					length,
					A2(_mgold$elm_random_pcg$Random_Pcg$int, 0, length)),
				seed));
		return _elm_lang$core$Basics$snd(
			_elm_lang$core$List$unzip(
				A2(
					_elm_lang$core$List$sortBy,
					_elm_lang$core$Basics$fst,
					A3(
						_elm_lang$core$List$map2,
						F2(
							function (v0, v1) {
								return {ctor: '_Tuple2', _0: v0, _1: v1};
							}),
						randomTags,
						list))));
	});
var _user$project$Update$applyPlaceFunction = F3(
	function (placeFunction, size, model) {
		var newStashAmount = A2(_user$project$Model$stashGet, size, model.stash) - 1;
		var newBoard = A2(placeFunction, size, model.board);
		var newModel = _elm_lang$core$Native_Utils.update(
			model,
			{
				board: newBoard,
				stash: A3(_user$project$Model$stashSet, size, newStashAmount, model.stash),
				selected: (_elm_lang$core$Native_Utils.cmp(newStashAmount, 1) > -1) ? model.selected : _elm_lang$core$Maybe$Nothing
			});
		return _elm_lang$core$Native_Utils.update(
			newModel,
			{
				outcome: _user$project$Model$getOutcome(newModel)
			});
	});
var _user$project$Update$applyMove = F2(
	function (model, move) {
		var _p0 = move;
		if (_p0.ctor === 'BoardMove') {
			return A3(
				_user$project$Update$applyPlaceFunction,
				_user$project$Model$place(_p0._0._0),
				_p0._0._1,
				model);
		} else {
			return A3(
				_user$project$Update$applyPlaceFunction,
				_user$project$Model$placeOnEdge(_p0._0._0),
				_p0._0._1,
				model);
		}
	});
var _user$project$Update$cpuWinningMove = function (model) {
	return function (_p1) {
		return A2(
			F2(
				function (x, y) {
					return _elm_lang$core$Native_Utils.eq(x, y);
				}),
			_user$project$Model$CPUWin,
			_user$project$Model$getOutcome(
				A2(_user$project$Update$applyMove, model, _p1)));
	};
};
var _user$project$Update$playerWinningMove = F2(
	function (model, move) {
		return function (_p2) {
			return A2(
				F2(
					function (x, y) {
						return _elm_lang$core$Native_Utils.eq(x, y);
					}),
				_user$project$Model$UserWin,
				_user$project$Model$getOutcome(
					A2(_user$project$Update$applyMove, model, _p2)));
		}(move);
	});
var _user$project$Update$EdgeMove = function (a) {
	return {ctor: 'EdgeMove', _0: a};
};
var _user$project$Update$BoardMove = function (a) {
	return {ctor: 'BoardMove', _0: a};
};
var _user$project$Update$getMoves = function (model) {
	var availableEdgeIdSizePairs = A2(_user$project$Model$getAvailableEdgeIdSizePairs, model.board, model.stash);
	var availableBoardIdSizePairs = A2(_user$project$Model$getAvailableBoardIdSizePairs, model.board, model.stash);
	return A2(
		_user$project$Update$shuffle,
		_mgold$elm_random_pcg$Random_Pcg$initialSeed(42),
		A2(
			_elm_lang$core$Basics_ops['++'],
			A2(_elm_lang$core$List$map, _user$project$Update$BoardMove, availableBoardIdSizePairs),
			A2(_elm_lang$core$List$map, _user$project$Update$EdgeMove, availableEdgeIdSizePairs)));
};
var _user$project$Update$nonLosingMove = F2(
	function (model, move) {
		var m = A2(_user$project$Update$applyMove, model, move);
		var potentialFuture = _elm_lang$core$Native_Utils.update(
			m,
			{player: _user$project$Model$User});
		var potentialFutureMoves = _user$project$Update$getMoves(potentialFuture);
		var _p3 = A2(
			_user$project$Extras$find,
			_user$project$Update$playerWinningMove(potentialFuture),
			potentialFutureMoves);
		if (_p3.ctor === 'Just') {
			return false;
		} else {
			return true;
		}
	});
var _user$project$Update$cpuTurn = function (model) {
	if (_elm_lang$core$Native_Utils.eq(model.outcome, _user$project$Model$TBD) && _elm_lang$core$Native_Utils.eq(model.player, _user$project$Model$CPU)) {
		var moves = _user$project$Update$getMoves(model);
		return A2(
			_elm_lang$core$Maybe$map,
			_user$project$Update$applyMove(model),
			A2(
				_user$project$Extras$orElseLazy,
				function (_p4) {
					var _p5 = _p4;
					return _elm_lang$core$Basics$fst(
						A2(
							_mgold$elm_random_pcg$Random_Pcg$step,
							_mgold$elm_random_pcg$Random_Pcg$sample(moves),
							_mgold$elm_random_pcg$Random_Pcg$initialSeed(42)));
				},
				A2(
					_user$project$Extras$orElseLazy,
					function (_p6) {
						var _p7 = _p6;
						return A2(
							_user$project$Extras$find,
							_user$project$Update$nonLosingMove(model),
							moves);
					},
					A2(
						_user$project$Extras$find,
						_user$project$Update$cpuWinningMove(model),
						moves))));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _user$project$Update$placeMap = F2(
	function (placeFunction, model) {
		var _p8 = model.selected;
		if (_p8.ctor === 'Just') {
			var _p11 = _p8._0;
			if (_elm_lang$core$Native_Utils.eq(model.outcome, _user$project$Model$TBD) && (_elm_lang$core$Native_Utils.cmp(
				A2(_user$project$Model$stashGet, _p11, model.stash),
				1) > -1)) {
				var postPlacementModel = A3(_user$project$Update$applyPlaceFunction, placeFunction, _p11, model);
				var newModel = _elm_lang$core$Native_Utils.update(
					postPlacementModel,
					{player: _user$project$Model$CPU});
				var postCPUTurnModel = function () {
					var _p9 = _user$project$Update$cpuTurn(newModel);
					if (_p9.ctor === 'Just') {
						return _p9._0;
					} else {
						return _elm_lang$core$Native_Utils.update(
							newModel,
							{
								outcome: function () {
									var _p10 = newModel.outcome;
									if (_p10.ctor === 'TBD') {
										return _user$project$Model$UserWin;
									} else {
										return _p10;
									}
								}()
							});
					}
				}();
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						postCPUTurnModel,
						{player: _user$project$Model$User}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			} else {
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{selected: _elm_lang$core$Maybe$Nothing}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			}
		} else {
			return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
		}
	});
var _user$project$Update$update = F2(
	function (msg, model) {
		var _p12 = msg;
		switch (_p12.ctor) {
			case 'NoOp':
				return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
			case 'NewGame':
				return {ctor: '_Tuple2', _0: _user$project$Model$defaultState, _1: _elm_lang$core$Platform_Cmd$none};
			case 'Select':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{selected: _p12._0}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'Place':
				return A2(
					_user$project$Update$placeMap,
					_user$project$Model$place(_p12._0),
					model);
			case 'PlaceOnEdge':
				return A2(
					_user$project$Update$placeMap,
					_user$project$Model$placeOnEdge(_p12._0),
					model);
			default:
				return A2(_debois$elm_mdl$Material$update, _p12._0, model);
		}
	});

var _user$project$TicTacDoh$subscriptions = _elm_lang$core$Basics$always(_elm_lang$core$Platform_Sub$none);
var _user$project$TicTacDoh$init = {ctor: '_Tuple2', _0: _user$project$Model$defaultState, _1: _elm_lang$core$Platform_Cmd$none};
var _user$project$TicTacDoh$main = {
	main: _elm_lang$html$Html_App$program(
		{init: _user$project$TicTacDoh$init, update: _user$project$Update$update, subscriptions: _user$project$TicTacDoh$subscriptions, view: _user$project$View$view})
};

var Elm = {};
Elm['TicTacDoh'] = Elm['TicTacDoh'] || {};
_elm_lang$core$Native_Platform.addPublicModule(Elm['TicTacDoh'], 'TicTacDoh', typeof _user$project$TicTacDoh$main === 'undefined' ? null : _user$project$TicTacDoh$main);

if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);

