# concurrence_demos
Some demos of how concurrence (parallel_ programming) is done in different languages. Basically a collection of cleaned-up code done before for some pupose. At the moment demoes illustrate two classic examples - dining philosophers and protected resource.

## Languages
The following language-specific demos are planned/provided:

### Ada
Ada has inbuilt concurrence handling at the level of the language. The paradigm is rather different from most other languages, as the concurrent features are designed as part of the language itself, and not just as a library calls attached to the core. All standard primitives (semaphores, resources, protected code blocks) can either be mapped directly to a language feature or constructed from provided language constructs.. Needs initial reading to get into the spirit, but provides most protection via proper feature design, as is typical of Ada.
  - [Dining philosophers](ada_dinphil/) TBD
  - [Protected resource](ada_protrsrc/) TBD
  
### C/C++
There is no support of concurrence/parallellizm at the language level at all. Not even via STL or analog. Moreover, specific mechanisms are highly platform-dependent and, as C/C++ is a low-level languae (essentially a glorified macroassembler, as far as such basic features are concerned), providing a single high-level ("thick") library would be against the design purpose of this language. Instead, platform-specific libs or calls have to be used. 

Here, provided demos cover the basic fork model (low-level Unix API) and pthreads library..

TBD


### Java
Java core language is rather oblivious to concurrence constructs, but it comes with a very extensive standard library, which provides the means to do parallel programming in it. Pretty much all standard parallel primitives are provided and documented, allowing to do most of the necessary tasks using just the standard API..
  - [Dining philosophers](java_dinphil) TBD
  - [Protected resource](java_protrsrc/) TBD


### Python
Similar to Java, Python supports most concurrence priitives via its (also very extensive) standard library. Also well documents and extensive, most simple tasks can be done using just standard resources.
  - [Dining philosophers](py_dinphil) TBD
  - [Protected resource](py_protrsrc/) TBD

