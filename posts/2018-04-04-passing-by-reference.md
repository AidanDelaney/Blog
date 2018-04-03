---
title: C++ pass by reference
author: Aidan Delaney
---

I introduced two changes into my current Intro to C++ course for first year undergraduates.  The first is the open access text book.  The second is in introducing _functions_ before I even introduce _conditionals_ and _iteration_.  By introducing functions first I think I've introduced abstraction and generalisation and changed how students see code.  We're now half way through the course and I added a small chapter to the open access book on passing parameters by reference.

This is pretty cool.  The book now corresponds **exactly** to how I want to teach the course (and not the other way around).  It's also great because I never have to introduce raw pointers in CS1.  I'll introduce smart pointers, but only students who go on to specialise in computing or engineering need to grasp the C++ memory model in second year.  My first year students, CS, engineering, maths, accounting and education people, can concentrate on [computational thinking](https://computationalthinkingcourse.withgoogle.com/) over and above the semantics of pointee life-cycles.

## Passing `int`

Consider the following function:

```c++
int increment(int x) {
  x = x + 1;
  return x;
}
```

We may call it from `main` (or any other function) as follows:

```c++
int main () {
  int my_val(0);

  increment(my_val);
  
  return 0;
}
```

In the above code we make 2 copies of the `int`.  We originally declare and instantiate `my_val`.  In the call to `increment` we create a copy of the `my_val` and pass it to the function.  In the function we modify the copy and then create a new copy to return to `main`.  This is because we are passing the `int` and returning the `int` by copy.

In the case of an `int` we're generally happy to pass it by copy.  An `int` is generally 4 bytes (32 bits) in size.  It's cheap and easy to copy an `int`.  However, consider the following:

```c++
const int COPIES = 1024 * 1024;

int main() {
	std::vector<std::string> strings;
	std::string eight_letter_word("jazzlike");
	
	for(int i=0; i<COPIES; i++) {
		strings.push_back(eight_letter_word);
	}
	
	// strings is now 8MB in size.
	
	return 0;
}
```

In the above code snippet we deliberately create a large chunk of memory.  After the `for` loop, the variable `strings` occupies about 8Mb of memory.  When passing this to a function we don't want to have to create another 8Mb copy of the variable.  It would be much nicer if we could pass some kind of **reference** to `strings`.

## Pass by reference

Passing by reference is denoted using the `&` symbol.  So when you see `int &` you read it as an `int` reference.  So, the following method takes parameters by copy:

```c++
int square(int x);
```

whereas the next snippet passes the parameter by reference:

```c++
int square(int & x);
```

You can mix and match how you pass parameters in a function call.  The following snippet passes one parameter by reference but another by copy:

```c++
int example(int & x, int y);
```

We pass parameters by reference if we wish to modify them.  We pass large things by **const reference** when we don't wish to modify them:

```c++
int example(const std::vector<std::string> vs);
```

In the above snippet the `const` precludes any modifications of `vs`.  That is to say, we can't call `vs.push_back("")` in the `example` method as that would modify `vs`.  We can read the size of `vs` doing `vs.size()` as that operation does not violate `const`-ness.

## Return by reference

As a rule, don't do this.  You can break this rule of thumb if you are return a reference that has been passed to the function.

One reason that we don't do this is to avoid returning a reference to a deleted local variable.  Consider the following function which returns an `int` reference:

```c++
int & square(const int x) {
	int square(x*x);
	return square;
}
```

If I compile it on a modern C++ compiler I get a warning.  However, this warning doesn't stop the code from compiling.

```bash
$ g++ -c return-ref.cpp 
return-ref.cpp: In function 'int& square(int)':
return-ref.cpp:2:6: warning: reference to local variable 'square' returned [-Wreturn-local-addr]
  int square(x*x);
      ^~~~~~
```

If we used the reference returned from `square` in `main` then the behaviour is undefined.  In the best case your application will crash.

## What we can now do

We can no pass large structures by reference or by `const` reference.

```c++
#include <iostream>
#include <vector>
#include <string>

int square(std::vector<std::string> & log, const int x) {
	std::string message("squaring value " + x);
	log.push_back(message);
	return x*x;
}

int main() {
	std::vector<std::string> log;
	int x(0);
	
	x = square(log, x);
	
	return 0;
}
```

