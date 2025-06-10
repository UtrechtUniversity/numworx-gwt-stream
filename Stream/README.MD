# Stream
This is Stream, a tool that helps students learn how to program, by allowing them to make flowcharts without paying attention to syntax. It is currently used by [Informatica-Actief](https://www.informatica-actief.nl/). The idea is that a student can work out a general picture of the algorithm that it wants to implement, before diving into the technical details. The first iteration of this project was part of a bachelors thesis. More on the philosophy behind this tool can be found [there](https://www.cs.ru.nl/bachelors-theses/2018/Frank_Gerlings___4384873___Digital_flowchart_maker.pdf). This readme will further focus on how to install the tool locally and continue development or run maintenance.

## Getting Started
To run the tool on your local machine you must acquire all the files. Do so by for example downloading the Git repo:
```
git clone https://github.com/gitLabor8/Stream.git
```
Now simply open `Stream.html` with a browser, preferably Chrome. Publishing the tool on a website only needs `Stream.html` and `main.js`. 

Note: `main.js` is only pushed very seldomly to the master branch (and the master branch only) for [git-technical reasons](https://kentcdodds.com/blog/why-i-dont-commit-generated-files-to-master). If you do not have the `main.js` file, you can generate one yourself by following the instructions below.

## Installing
In order to tinker around with the source files, you need an [Elm compiler](https://guide.elm-lang.org/install.html). This way we can create a new `main.js` based on the source files. Don't worry! The compiler will out the depencies by itself, so this will be relatively easy. With Elm installed, just run:
```
elm make src/Main.elm --output=main.js --debug
```
Or, if you can run bash scripts, run:
```
./build.sh
```
to compile the project in debug mode and automatically open a browser (recommended during development) or
```
./ship.sh
```
when you want to ship the project without debug instances. From that point onwards you only need to copy `Stream.html` and `main.js` to the folder where you want to host the tool.

## Code structure
We will now dive into the purpose of every class, but first get a grasp of the general code structure:

![codeStructure](https://github.com/gitLabor8/Stream/blob/debug/Stream_dependency_chart.jpg "Overview code structure")

It should be noted that Elm naturally has a [Model-Update-View architecture](https://guide.elm-lang.org/architecture/). We will categorise the classes as such.

### Core
`tree/core.elm` is the purely mathematical tree structure.

### State
`tree/state.elm` keeps track of, you guessed it, the state of the whole application. Aside from just the tree, it also keeps track of peripheral information, such as the name of the flowchart, the contents of the pre- and postconditions and what node to highlight. It also does the updating. In order to do so it defines a multitude of auxiliary types.

### Draw
`tree/draw.elm` functions as the view of the tree. It draws all the different elements pretty much line by line, making this a very bulky file that does a lot of relatively less interesting stuff.

### Save
`save.elm` takes care of the menu at the bottom of the application. It has its seperate Model-Update-View built in. The most interesting stuff in this file is the encoding and decoding to and from JSON.

### Ports
`ports.elm` is used to communicate with the JavaScript/HTML wrapper in `Stream.html`. It was necessary, since Elm had difficulties with file upload and because creating a modal to show the Java comments was relatively easy in JavaScript. Currently, Elm is capable of doing all these things without JavaScript. Therefore this class will be refactored/scrapped in the long run.

### Main
`main.elm` combines the states and views of `tree/` and `save.elm`. This file mostly consists of through looping of messages.

### Stream.html
`Stream.html` is the HTML-wrapper for the tool. It reads `main.js`, the elm-generated file that you absolutely don't want to look at up close. Currently it still prompts the modal that shows the Java comments.

## Authors
Hi! My name is [Frank Gerlings](https://www.linkedin.com/in/frank-gerlings/) and I made this tool on my own (aside from some help that I'll mention below). If you want to contribute to this project, please do contact me via GitHub or LinkedIn!

## Acknowledgments
This project wouldn't have been possible without the help of:
* Sjaak Smetsers and Tim Steenvoorden, my bachelor thesis supervisors, for their initial ideas, technical help and support along the first iteration
* Renske Smetsers-Weeda for field testing
* Paul Bergervoet and Informatica-Actief as a whole for funding further development

