<div class="section" id="haskell">
<h2>Haskell<a class="headerlink" href="#haskell" title="Permalink to this headline">¶</a></h2>
<p>The most important consideration is, as usual, to stay consistent with the
existing code.</p>
<p>As there’s no “canonical” style guide for Haskell, this code style has been
inspired from a few online resources, including the style guide for the
<a class="reference external" href="http://snapframework.com/docs/style-guide">Snap framework</a>, <a class="reference external" href="https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md">this style guide</a> and <a class="reference external" href="http://www.cs.caltech.edu/courses/cs11/material/haskell/misc/haskell_style_guide.html">this other style guide</a>.</p>
<div class="section" id="files">
<h3>Files<a class="headerlink" href="#files" title="Permalink to this headline">¶</a></h3>
<p>Use ordinary, non-<a class="reference external" href="http://www.haskell.org/haskellwiki/Literate_programming">literate</a> Haskell <tt class="docutils literal"><span class="pre">.hs</span></tt> files.</p>
<p>Use proper copyright headers, and proper Haddock style documentation headers:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="cm">{-| Short module summary.</span>

<span class="cm">Longer module description.</span>

<span class="cm">-}</span>

<span class="cm">{-</span>

<span class="cm">Copyright (C) ...</span>

<span class="cm">This program is free software ...</span>

<span class="cm">-}</span>
</pre></div>
</div>
<p>If there are module-level pragmas add them right at the top, before the short
summary.</p>
</div>
<div class="section" id="imports">
<h3>Imports<a class="headerlink" href="#imports" title="Permalink to this headline">¶</a></h3>
<p>Imports should be grouped into the following groups and inside each group they
should be sorted alphabetically:</p>
<ol class="arabic simple">
<li>import of non-Ganeti libaries</li>
<li>import of Ganeti libraries</li>
</ol>
<p>It is allowed to use qualified imports with short names for:</p>
<ul class="simple">
<li>standard library (e.g. <tt class="docutils literal"><span class="pre">import</span> <span class="pre">qualified</span> <span class="pre">Data.Map</span> <span class="pre">as</span> <span class="pre">M</span></tt>)</li>
<li>local imports (e.g. <tt class="docutils literal"><span class="pre">import</span> <span class="pre">qualified</span> <span class="pre">Ganeti.Constants</span> <span class="pre">as</span> <span class="pre">C</span></tt>)</li>
</ul>
<p>Whenever possible, prefer explicit imports, either in form of
qualified imports, or by naming the imported functions
(e.g., <tt class="docutils literal"><span class="pre">import</span> <span class="pre">Control.Arrow</span> <span class="pre">((&amp;&amp;&amp;))</span></tt>, <tt class="docutils literal"><span class="pre">import</span> <span class="pre">Data.Foldable(fold,</span> <span class="pre">toList)</span></tt>)</p>
</div>
<div class="section" id="id1">
<h3>Indentation<a class="headerlink" href="#id1" title="Permalink to this headline">¶</a></h3>
<p>Use only spaces, never tabs. Indentation level is 2 characters. For Emacs,
this means setting the variable <tt class="docutils literal"><span class="pre">haskell-indent-offset</span></tt> to 2.</p>
<p>Line length should be at most 78 chars, and 72 chars inside comments.</p>
<p>Use indentation-based structure, and not braces/semicolons.</p>
<div class="admonition note">
<p class="first admonition-title">Note</p>
<p>Special indendation of if/then/else construct</p>
<p>For the <tt class="docutils literal"><span class="pre">do</span></tt> notation, the <tt class="docutils literal"><span class="pre">if-then-else</span></tt> construct has a non-intuitive
behaviour. As such, the indentation of <tt class="docutils literal"><span class="pre">if-then-else</span></tt> (both in <tt class="docutils literal"><span class="pre">do</span></tt>
blocks and in normal blocks) should be as follows:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="kr">if</span> <span class="n">condition</span>
  <span class="kr">then</span> <span class="n">expr1</span>
  <span class="kr">else</span> <span class="n">expr2</span>
</pre></div>
</div>
<p class="last">i.e. indent the then/else lines with another level. This can be accomplished
in Emacs by setting the variable <tt class="docutils literal"><span class="pre">haskell-indent-thenelse</span></tt> to 2 (from the
default of zero).</p>
</div>
<p>If you have more than one line of code please newline/indent after the “=”. Do
<cite>not</cite> do:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="nf">f</span> <span class="n">x</span> <span class="ow">=</span> <span class="kr">let</span> <span class="n">y</span> <span class="ow">=</span> <span class="n">x</span> <span class="o">+</span> <span class="mi">1</span>
      <span class="kr">in</span>  <span class="n">y</span>
</pre></div>
</div>
<p>Instead do:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="nf">f</span> <span class="n">x</span> <span class="ow">=</span>
  <span class="kr">let</span> <span class="n">y</span> <span class="ow">=</span> <span class="n">x</span> <span class="o">+</span> <span class="mi">1</span>
  <span class="kr">in</span>  <span class="n">y</span>
</pre></div>
</div>
<p>or if it is just one line:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="nf">f</span> <span class="n">x</span> <span class="ow">=</span> <span class="n">x</span> <span class="o">+</span> <span class="mi">1</span>
</pre></div>
</div>
</div>
<div class="section" id="multiline-strings">
<h3>Multiline strings<a class="headerlink" href="#multiline-strings" title="Permalink to this headline">¶</a></h3>
<p>Multiline strings are created by closing a line with a backslash and starting
the following line with a backslash, keeping the indentation level constant.
Whitespaces go on the new line, right after the backslash.</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="nf">longString</span> <span class="ow">::</span> <span class="kt">String</span>
<span class="nf">longString</span> <span class="ow">=</span> <span class="s">"This is a very very very long string that</span><span class="se">\</span>
<span class="se">             \</span><span class="s"> needs to be split in two lines"</span>
</pre></div>
</div>
</div>
<div class="section" id="data-declarations">
<h3>Data declarations<a class="headerlink" href="#data-declarations" title="Permalink to this headline">¶</a></h3>
<div class="admonition warning">
<p class="first admonition-title">Warning</p>
<p class="last">Note that this is different from the Python style!</p>
</div>
<p>When declaring either data types, or using list literals, etc., the columns
should be aligned, and for lists use a comma at the start of the line, not at
the end. Examples:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="kr">data</span> <span class="kt">OpCode</span> <span class="ow">=</span> <span class="kt">OpStartupInstance</span> <span class="o">...</span>
            <span class="o">|</span> <span class="kt">OpShutdownInstance</span> <span class="o">...</span>
            <span class="o">|</span> <span class="o">...</span>

<span class="kr">data</span> <span class="kt">Node</span> <span class="ow">=</span> <span class="kt">Node</span> <span class="p">{</span> <span class="n">name</span> <span class="ow">::</span> <span class="kt">String</span>
                 <span class="p">,</span> <span class="n">ip</span>   <span class="ow">::</span> <span class="kt">String</span>
                 <span class="p">,</span> <span class="o">...</span>
                 <span class="p">}</span>

<span class="nf">myList</span> <span class="ow">=</span> <span class="p">[</span> <span class="n">value1</span>
         <span class="p">,</span> <span class="n">value2</span>
         <span class="p">,</span> <span class="n">value3</span>
         <span class="p">]</span>
</pre></div>
</div>
<p>The choice of whether to wrap the first element or not is up to you; the
following is also allowed:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="nf">myList</span> <span class="ow">=</span>
  <span class="p">[</span> <span class="n">value1</span>
  <span class="p">,</span> <span class="n">value2</span>
  <span class="p">]</span>
</pre></div>
</div>
<p>For records, always add spaces around the braces and the equality sign.</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="nf">foo</span> <span class="ow">=</span> <span class="kt">Foo</span> <span class="p">{</span> <span class="n">fBar</span> <span class="ow">=</span> <span class="s">"bar"</span><span class="p">,</span> <span class="n">fBaz</span> <span class="ow">=</span> <span class="mi">4711</span> <span class="p">}</span>

<span class="nf">foo'</span> <span class="ow">=</span> <span class="kt">Foo</span> <span class="p">{</span> <span class="n">fBar</span> <span class="ow">=</span> <span class="s">"bar 2"</span>
           <span class="p">,</span> <span class="n">fBaz</span> <span class="ow">=</span> <span class="mi">4712</span>
           <span class="p">}</span>

<span class="nf">node'</span> <span class="ow">=</span> <span class="n">node</span> <span class="p">{</span> <span class="n">ip</span> <span class="ow">=</span> <span class="s">"127.0.0.1"</span> <span class="p">}</span>
</pre></div>
</div>
</div>
<div class="section" id="white-space">
<h3>White space<a class="headerlink" href="#white-space" title="Permalink to this headline">¶</a></h3>
<p>Like in Python, surround binary operators with one space on either side. Do no
insert a space after a lamda:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="c1">-- bad</span>
<span class="nf">map</span> <span class="p">(</span><span class="nf">\</span> <span class="n">n</span> <span class="ow">-&gt;</span> <span class="o">...</span><span class="p">)</span> <span class="n">lst</span>
<span class="c1">-- good</span>
<span class="nf">foldl</span> <span class="p">(</span><span class="nf">\</span><span class="n">x</span> <span class="n">y</span> <span class="ow">-&gt;</span> <span class="o">...</span><span class="p">)</span> <span class="o">...</span>
</pre></div>
</div>
<p>Use a blank line between top-level definitions, but no blank lines between
either the comment and the type signature or between the type signature and
the actual function definition.</p>
<div class="admonition note">
<p class="first admonition-title">Note</p>
<p class="last">Ideally it would be two blank lines between top-level definitions, but the
code only has one now.</p>
</div>
<p>As always, no trailing spaces. Ever.</p>
<div class="section" id="spaces-after-comma">
<h4>Spaces after comma<a class="headerlink" href="#spaces-after-comma" title="Permalink to this headline">¶</a></h4>
<p>Instead of:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="p">(</span><span class="s">"a"</span><span class="p">,</span><span class="s">"b"</span><span class="p">)</span>
</pre></div>
</div>
<p>write:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="p">(</span><span class="s">"a"</span><span class="p">,</span> <span class="s">"b"</span><span class="p">)</span>
</pre></div>
</div>
</div>
</div>
<div class="section" id="naming">
<h3>Naming<a class="headerlink" href="#naming" title="Permalink to this headline">¶</a></h3>
<p>Functions should be named in mixedCase style, and types in CamelCase. Function
arguments and local variables should be mixedCase.</p>
<p>When using acronyms, ones longer than 2 characters should be typed capitalised,
not fully upper-cased (e.g. <tt class="docutils literal"><span class="pre">Http</span></tt>, not <tt class="docutils literal"><span class="pre">HTTP</span></tt>).</p>
<p>For variable names, use descriptive names; it is only allowed to use very
short names (e.g. <tt class="docutils literal"><span class="pre">a</span></tt>, <tt class="docutils literal"><span class="pre">b</span></tt>, <tt class="docutils literal"><span class="pre">i</span></tt>, <tt class="docutils literal"><span class="pre">j</span></tt>, etc.) when:</p>
<ul>
<li><p class="first">the function is trivial, e.g.:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="nf">sum</span> <span class="n">x</span> <span class="n">y</span> <span class="ow">=</span> <span class="n">x</span> <span class="o">+</span> <span class="n">y</span>
</pre></div>
</div>
</li>
<li><p class="first">we talk about some very specific cases, e.g.
iterators or accumulators in folds:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="nf">map</span> <span class="p">(</span><span class="nf">\</span><span class="n">v</span> <span class="ow">-&gt;</span> <span class="n">v</span> <span class="o">+</span> <span class="mi">1</span><span class="p">)</span> <span class="n">lst</span>
</pre></div>
</div>
</li>
<li><p class="first">using <tt class="docutils literal"><span class="pre">x:xs</span></tt> for list elements and lists, etc.</p>
</li>
</ul>
<p>In general, short/one-letter names are allowed when we deal with polymorphic
values; for example the standard map definition from Prelude:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="nf">map</span> <span class="ow">::</span> <span class="p">(</span><span class="n">a</span> <span class="ow">-&gt;</span> <span class="n">b</span><span class="p">)</span> <span class="ow">-&gt;</span> <span class="p">[</span><span class="n">a</span><span class="p">]</span> <span class="ow">-&gt;</span> <span class="p">[</span><span class="n">b</span><span class="p">]</span>
<span class="nf">map</span> <span class="kr">_</span> <span class="kt">[]</span>     <span class="ow">=</span> <span class="kt">[]</span>
<span class="nf">map</span> <span class="n">f</span> <span class="p">(</span><span class="n">x</span><span class="kt">:</span><span class="n">xs</span><span class="p">)</span> <span class="ow">=</span> <span class="n">f</span> <span class="n">x</span> <span class="kt">:</span> <span class="n">map</span> <span class="n">f</span> <span class="n">xs</span>
</pre></div>
</div>
<p>In this example, neither the <tt class="docutils literal"><span class="pre">a</span></tt> nor <tt class="docutils literal"><span class="pre">b</span></tt> types are known to the map
function, so we cannot give them more explicit names. Since the body of the
function is trivial, the variables used are longer.</p>
<p>However, if we deal with explicit types or values, their names should be
descriptive.</p>
<p>Finally, the naming should look familiar to people who just read the
Prelude/standard libraries.</p>
<div class="section" id="naming-for-updated-values">
<h4>Naming for updated values<a class="headerlink" href="#naming-for-updated-values" title="Permalink to this headline">¶</a></h4>
<p>Since one cannot update a value in Haskell, this presents a particular problem
on the naming of new versions of the same value. For example, the following
code in Python:</p>
<div class="highlight-python"><div class="highlight"><pre><span class="k">def</span> <span class="nf">failover</span><span class="p">(</span><span class="n">pri</span><span class="p">,</span> <span class="n">sec</span><span class="p">,</span> <span class="n">inst</span><span class="p">):</span>
  <span class="n">pri</span><span class="o">.</span><span class="n">removePrimary</span><span class="p">(</span><span class="n">inst</span><span class="p">)</span>
  <span class="n">pri</span><span class="o">.</span><span class="n">addSecondary</span><span class="p">(</span><span class="n">inst</span><span class="p">)</span>
  <span class="n">sec</span><span class="o">.</span><span class="n">removeSecondary</span><span class="p">(</span><span class="n">inst</span><span class="p">)</span>
  <span class="n">sec</span><span class="o">.</span><span class="n">addPrimary</span><span class="p">(</span><span class="n">inst</span><span class="p">)</span>
</pre></div>
</div>
<p>becomes in Haskell something like the following:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="nf">failover</span> <span class="n">pri</span> <span class="n">sec</span> <span class="n">inst</span> <span class="ow">=</span>
  <span class="kr">let</span> <span class="n">pri'</span>  <span class="ow">=</span> <span class="n">removePrimary</span> <span class="n">pri</span> <span class="n">inst</span>
      <span class="n">pri''</span> <span class="ow">=</span> <span class="n">addSecondary</span> <span class="n">pri'</span> <span class="n">inst</span>
      <span class="n">sec'</span>  <span class="ow">=</span> <span class="n">removeSecondary</span> <span class="n">sec</span> <span class="n">inst</span>
      <span class="n">sec''</span> <span class="ow">=</span> <span class="n">addPrimary</span> <span class="n">sec'</span> <span class="n">inst</span>
  <span class="kr">in</span> <span class="p">(</span><span class="n">pri''</span><span class="p">,</span> <span class="n">sec''</span><span class="p">)</span>
</pre></div>
</div>
<p>When updating values, one should add single quotes to the name for up to three
new names (e.g. <tt class="docutils literal"><span class="pre">inst</span></tt>, <tt class="docutils literal"><span class="pre">inst'</span></tt>, <tt class="docutils literal"><span class="pre">inst''</span></tt>, <tt class="docutils literal"><span class="pre">inst'''</span></tt>) and otherwise
use numeric suffixes (<tt class="docutils literal"><span class="pre">inst1</span></tt>, <tt class="docutils literal"><span class="pre">inst2</span></tt>, <tt class="docutils literal"><span class="pre">inst3</span></tt>, ..., <tt class="docutils literal"><span class="pre">inst8</span></tt>), but
that many updates is already bad style and thus should be avoided.</p>
</div>
</div>
<div class="section" id="type-signatures">
<h3>Type signatures<a class="headerlink" href="#type-signatures" title="Permalink to this headline">¶</a></h3>
<p>Always declare types for functions (and any other top-level bindings).</p>
<p>If in doubt, feel free to declare the type of the variables/bindings in a
complex expression; this usually means the expression is too complex, however.</p>
<p>Similarly, provide Haddock-style comments for top-level definitions.</p>
</div>
<div class="section" id="use-sum-types-instead-of-exceptions">
<h3>Use sum types instead of exceptions<a class="headerlink" href="#use-sum-types-instead-of-exceptions" title="Permalink to this headline">¶</a></h3>
<p>Exceptions make it hard to write functional code, as alternative
control flows need to be considered and compiler support is limited.
Therefore, Ganeti functions should never allow exceptions to escape.
Function that can fail should report failure by returning an appropriate
sum type (<tt class="docutils literal"><span class="pre">Either</span></tt> or one of its glorified variants like <tt class="docutils literal"><span class="pre">Maybe</span></tt> or
<tt class="docutils literal"><span class="pre">Result</span></tt>); the preferred sum type for reporting errors is <tt class="docutils literal"><span class="pre">Result</span></tt>.</p>
<p>As other Ganeti functions also follow these guide lines, they can safely
be composed. However, be careful when using functions from other libraries;
if they can raise exceptions, catch them, preferably as close to their
origin as reasonably possible.</p>
</div>
<div class="section" id="parentheses-point-free-style">
<h3>Parentheses, point free style<a class="headerlink" href="#parentheses-point-free-style" title="Permalink to this headline">¶</a></h3>
<p>Prefer the so-called <a class="reference external" href="http://www.haskell.org/haskellwiki/Pointfree">point-free</a> style style when declaring functions, if
applicable:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="c1">-- bad</span>
<span class="kr">let</span> <span class="n">a</span> <span class="n">x</span> <span class="ow">=</span> <span class="n">f</span> <span class="p">(</span><span class="n">g</span> <span class="p">(</span><span class="n">h</span> <span class="n">x</span><span class="p">))</span>
<span class="c1">-- good</span>
<span class="kr">let</span> <span class="n">a</span> <span class="ow">=</span> <span class="n">f</span> <span class="o">.</span> <span class="n">g</span> <span class="o">.</span> <span class="n">h</span>
</pre></div>
</div>
<p>Also use function composition in a similar manner in expressions to avoid extra
parentheses:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="c1">-- bad</span>
<span class="nf">f</span> <span class="p">(</span><span class="n">g</span> <span class="p">(</span><span class="n">h</span> <span class="n">x</span><span class="p">))</span>
<span class="c1">-- better</span>
<span class="nf">f</span> <span class="o">$</span> <span class="n">g</span> <span class="o">$</span> <span class="n">h</span> <span class="n">x</span>
<span class="c1">-- best</span>
<span class="nf">f</span> <span class="o">.</span> <span class="n">g</span> <span class="o">.</span> <span class="n">h</span> <span class="o">$</span> <span class="n">x</span>
</pre></div>
</div>
</div>
<div class="section" id="language-features">
<h3>Language features<a class="headerlink" href="#language-features" title="Permalink to this headline">¶</a></h3>
<div class="section" id="extensions">
<h4>Extensions<a class="headerlink" href="#extensions" title="Permalink to this headline">¶</a></h4>
<p>It is recommended to keep the use of extensions to a minimum, so that the code
can be understood even if one is familiar with just Haskel98/Haskell2010. That
said, some extensions are very common and useful, so they are recommended:</p>
<ul class="simple">
<li><a class="reference external" href="http://www.haskell.org/ghc/docs/latest/html/users_guide/bang-patterns.html">Bang patterns</a>: useful when you want to enforce strict evaluation (and better
than repeated use of <tt class="docutils literal"><span class="pre">seq</span></tt>)</li>
<li>CPP: a few modules need this in order to account for configure-time options;
don’t overuse it, since it breaks multi-line strings</li>
<li><a class="reference external" href="http://www.haskell.org/ghc/docs/latest/html/users_guide/template-haskell.html">Template Haskell</a>: we use this for automatically deriving JSON instances and
other similar boiler-plate</li>
</ul>
<p>Such extensions should be declared using the <tt class="docutils literal"><span class="pre">Language</span></tt> pragma:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="cm">{-# Language BangPatterns #-}</span>

<span class="cm">{-| This is a small module... -}</span>
</pre></div>
</div>
</div>
<div class="section" id="comments">
<h4>Comments<a class="headerlink" href="#comments" title="Permalink to this headline">¶</a></h4>
<p>Always use proper sentences; start with a capital letter and use punctuation
in top level comments:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="c1">-- | A function that does something.</span>
<span class="nf">f</span> <span class="ow">::</span> <span class="o">...</span>
</pre></div>
</div>
<p>For inline comments, start with a capital letter but no ending punctuation.
Furthermore, align the comments together with a 2-space width from the end of
the item being commented:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="kr">data</span> <span class="kt">Maybe</span> <span class="n">a</span> <span class="ow">=</span> <span class="kt">Nothing</span>  <span class="c1">-- ^ Represents empty container</span>
             <span class="o">|</span> <span class="kt">Just</span> <span class="n">a</span>   <span class="c1">-- ^ Represents a single value</span>
</pre></div>
</div>
<p>The comments should be clear enough so that one doesn’t need to look at the
code to understand what the item does/is.</p>
<p>Use <tt class="docutils literal"><span class="pre">--</span> <span class="pre">|</span></tt> to write doc strings rather than bare comment with <tt class="docutils literal"><span class="pre">--</span></tt>.</p>
</div>
<div class="section" id="tools">
<h4>Tools<a class="headerlink" href="#tools" title="Permalink to this headline">¶</a></h4>
<p>We generate the API documentation via Haddock, and as such the comments should
be correct (syntax-wise) for it. Use markup, but sparingly.</p>
<p>We use <a class="reference external" href="http://community.haskell.org/~ndm/darcs/hlint/hlint.htm">hlint</a> as a lint checker; the code is currently lint-clean, so you must
not add any warnings/errors.</p>
<p>Use these two commands during development:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="nf">make</span> <span class="n">hs</span><span class="o">-</span><span class="n">apidoc</span>
<span class="nf">make</span> <span class="n">hlint</span>
</pre></div>
</div>
</div>
<div class="section" id="quickcheck-best-practices">
<h4>QuickCheck best practices<a class="headerlink" href="#quickcheck-best-practices" title="Permalink to this headline">¶</a></h4>
<p>If you have big type that takes time to generate and several properties to
test on that, by default 500 of those big instances are generated for each
property. In many cases, it would be sufficient to only generate those 500
instances once and test all properties on those. To do this, create a property
that uses <tt class="docutils literal"><span class="pre">conjoin</span></tt> to combine several properties into one. Use
<tt class="docutils literal"><span class="pre">counterexample</span></tt> to add expressive error messages. For example:</p>
<div class="highlight-haskell"><div class="highlight"><pre><span class="nf">prop_myMegaProp</span> <span class="ow">::</span> <span class="n">myBigType</span> <span class="ow">-&gt;</span> <span class="kt">Property</span>
<span class="nf">prop_myMegaProp</span> <span class="n">b</span> <span class="ow">=</span>
  <span class="n">conjoin</span>
    <span class="p">[</span> <span class="n">counterexample</span>
        <span class="p">(</span><span class="s">"Something failed horribly here: "</span> <span class="o">++</span> <span class="n">show</span> <span class="n">b</span><span class="p">)</span> <span class="p">(</span><span class="n">subProperty1</span> <span class="n">b</span><span class="p">)</span>
    <span class="p">,</span> <span class="n">counterexample</span>
        <span class="p">(</span><span class="s">"Something else failed horribly here: "</span> <span class="o">++</span> <span class="n">show</span> <span class="n">b</span><span class="p">)</span>
        <span class="p">(</span><span class="n">subProperty2</span> <span class="n">b</span><span class="p">)</span>
    <span class="p">,</span> <span class="c1">-- more properties here ...</span>
    <span class="p">]</span>

<span class="nf">subProperty1</span> <span class="ow">::</span> <span class="n">myBigType</span> <span class="ow">-&gt;</span> <span class="kt">Bool</span>
<span class="nf">subProperty1</span> <span class="n">b</span> <span class="ow">=</span> <span class="o">...</span>

<span class="nf">subProperty2</span> <span class="ow">::</span> <span class="n">myBigType</span> <span class="ow">-&gt;</span> <span class="kt">Property</span>
<span class="nf">subProperty2</span> <span class="n">b</span> <span class="ow">=</span> <span class="o">...</span>

<span class="o">...</span>
</pre></div>
</div>
<div class="section" id="maybe-generation">
<h5>Maybe Generation<a class="headerlink" href="#maybe-generation" title="Permalink to this headline">¶</a></h5>
<p>Use <tt class="docutils literal"><span class="pre">genMaybe</span> <span class="pre">genSomething</span></tt> to create <tt class="docutils literal"><span class="pre">Maybe</span></tt> instances of something
including some <tt class="docutils literal"><span class="pre">Nothing</span></tt> instances.</p>
<p>Use <tt class="docutils literal"><span class="pre">Just</span> <span class="pre">&lt;$&gt;</span> <span class="pre">genSomething</span></tt> to generate only <tt class="docutils literal"><span class="pre">Just</span></tt> instances of
something.</p>
</div>
<div class="section" id="string-generation">
<h5>String Generation<a class="headerlink" href="#string-generation" title="Permalink to this headline">¶</a></h5>
<p>To generate strings, consider using <tt class="docutils literal"><span class="pre">genName</span></tt> instead of <tt class="docutils literal"><span class="pre">arbitrary</span></tt>.
<tt class="docutils literal"><span class="pre">arbitrary</span></tt> has the tendency to generate strings that are too long.</p>
</div>
</div>
</div>
</div>
