var gulp            = require("gulp");
var purescript      = require("gulp-purescript");
var sass            = require("gulp-sass");
var browserify      = require("browserify");
var vinyl           = require("vinyl-source-stream")
var closureCompiler = require("gulp-closure-compiler");

// Purescript

var sources = [
  "src/**/*.purs",
  "test/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs",
];

var foreigns = [
  "src/**/*.js",
  "bower_components/purescript-*/src/**/*.js"
];

gulp.task("make", function () {
  return purescript.psc({
    src: sources,
    ffi: foreigns,
    output: "output",
    verboseErrors: false
  });
});

gulp.task("dotpsci", function () {
  return purescript.psci({ src: sources, ffi: foreigns })
    .pipe(gulp.dest("."));
});

gulp.task("bundle", ["make"], function () {
  return purescript.pscBundle({
    src: "output/**/*.js",
    output: "dist/main.js",
    main: "Main"
  });
});

gulp.task("browserify", ["bundle"], function () {
  return browserify("dist/main.js")
    .bundle()
    .pipe(vinyl("main.js"))
    .pipe(gulp.dest("public/js"));
});

// Sass

gulp.task("sass", function() {
  return gulp.src("sass/**/*.scss")
      .pipe(sass({
        errLogToConsole: true
      }))
      .pipe(gulp.dest("public/css"));
});

// Closure compiler

gulp.task("closureCompiler", ["browserify"], function() {
  return gulp.src("public/js/main.js")
    .pipe(closureCompiler({
      compilerPath: "bower_components/closure-compiler/compiler.jar",
      fileName: "public/js/main.min.js",
      compilerFlags: {
        language_in: "ECMASCRIPT5_STRICT"
      },
    }))
    .pipe(gulp.dest("public/js"));
});

// Handsontable

gulp.task("handsontable", function() {
  gulp.src("bower_components/handsontable/dist/handsontable.full.min.css")
    .pipe(gulp.dest("public/css/"));
  return gulp.src("bower_components/handsontable/dist/handsontable.full.min.js")
    .pipe(gulp.dest("public/js/"));
})

// Main tasks

gulp.task("default", ["browserify", "sass", "handsontable"]);

gulp.task("prod", ["default", "closureCompiler"])
