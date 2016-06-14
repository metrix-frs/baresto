var gulp = require('gulp')
var purescript = require('gulp-purescript')
var sass = require('gulp-sass')
var browserify = require('browserify')
var envify = require('envify')
var vinyl = require('vinyl-source-stream')
var uglify = require('gulp-uglify')
var cleanCSS = require('gulp-clean-css')
var browserSync = require('browser-sync')

// Purescript

var sources = [
  'src/**/*.purs',
  'bower_components/purescript-*/src/**/*.purs'
]

var foreigns = [
  'src/**/*.js',
  'bower_components/purescript-*/src/**/*.js'
]

gulp.task('make', function () {
  return purescript.psc({
    src: sources,
    ffi: foreigns,
    output: 'output',
    verboseErrors: false
  })
})

gulp.task('dotpsci', function () {
  return purescript.psci({ src: sources, ffi: foreigns })
    .pipe(gulp.dest('.'))
})

gulp.task('bundle', ['make'], function () {
  return purescript.pscBundle({
    src: 'output/**/*.js',
    output: 'dist/main.js',
    main: 'Main'
  })
})

gulp.task('browserify', ['bundle'], function () {
  if (process.env['API_URL'] === undefined) {
    console.error('Error: API_URL environment variable must be set ("" for development).')
    process.exit(1)
  }
  return browserify('dist/main.js')
    .transform(envify)
    .require(['moment', 'pikaday', 'zeroclipboard'])
    .bundle()
    .pipe(vinyl('main.js'))
    .pipe(gulp.dest('public/js'))
})

// Sass

gulp.task('sass', function () {
  return gulp.src('sass/**/*.scss')
    .pipe(sass({
      errLogToConsole: true
    }))
    .pipe(gulp.dest('public/css'))
})

// Handsontable

gulp.task('handsontable', function () {
  return gulp.src('node_modules/handsontable/dist/handsontable.full.min.css')
    .pipe(gulp.dest('public/css/'))
})

// Watch

gulp.task('watch', function () {
  browserSync.init(null, {
    proxy: 'http://localhost:3000',
    port: 3001,
    open: false
  })
  gulp.watch('public/js/main.js', browserSync.reload)
  gulp.watch('sass/**/*.scss', function () {
    gulp.src('sass/**/*.scss')
      .pipe(sass({
        errLogToConsole: true
      })).on('error', function (err) {
        console.log(err)
        this.emit('end')
      })
      .pipe(gulp.dest('public/css'))
      .pipe(browserSync.stream())
  })
})

// Main tasks

gulp.task('default', ['browserify', 'sass', 'handsontable'])

gulp.task('prod', ['default'], function () {
  gulp.src('public/css/main.css')
    .pipe(cleanCSS())
    .pipe(gulp.dest('public/css/'))
  return gulp.src('public/js/main.js')
    .pipe(uglify())
    .pipe(gulp.dest('public/js/'))
})
