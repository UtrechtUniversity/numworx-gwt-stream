# Stream
This is Stream, a tool that helps students learn how to program, by allowing them to make flowcharts. It is currently used by [Informatica-Actief](https://www.informatica-actief.nl/). The idea is that a student can work out a general picture of the algorithm that it wants to implement, before diving into the technical details. The first iteration of this project was part of a bachelors thesis. More on the philosophy behind this tool can be found [there](https://www.cs.ru.nl/bachelors-theses/2018/Frank_Gerlings___4384873___Digital_flowchart_maker.pdf). This readme will further focus on how to install it locally and continue development/run maintenance.

## Getting Started
To copy the project and run it on your local machine acquire all the files by for example downloading the Git repo:
```
git clone https://github.com/gitLabor8/Stream.git
```
Now simply open `Stream.html` with a browser, preferably Chrome. Publishing the tool on a website only needs `Stream.html` and `main.js`. 

### Installing
In order to tinker around with the source files, you need an [Elm compiler](https://guide.elm-lang.org/install.html). This way we can create a new `main.js` based on the source files. Don't worry! The compiler will read `elm.json` and figure out the depencies. Just run:
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
when you want to ship the project without debug instances.

## Code structure

## Authors
Hi! My name is [Frank Gerlings](https://www.linkedin.com/in/frank-gerlings/) and I made this tool on my own (aside from some help that I'll mention below). If you want to contribute to this project, please do contact me via GitHub or LinkedIn!

## Acknowledgments
This project wouldn't have been possible without the help of:
* Sjaak Smetsers and Tim Steenvoorden, my bachelor thesis supervisors, for their initial ideas, technical help and support along the first iteration
* Renske Smetsers-Weeda for field testing
* Paul Bergervoet and Informatica-Actief as a whole for funding further development

