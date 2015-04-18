from bottle import route, run, debug, template, request, static_file
from flask import jsonify

def is_number(s):
  try:
    float(s)
    return True
  except ValueError:
    return False

cities = ['boston', 'nyc', 'philly']

# Static Routes
@route('/<filename:re:.*\.js>')
def javascripts(filename):
    return static_file(filename, root='./static/js')

@route('/<filename:re:.*\.css>')
def stylesheets(filename):
    return static_file(filename, root='./static/css')

@route('/<filename:re:.*\.(jpg|png|gif|ico)>')
def images(filename):
    return static_file(filename, root='./static/img')

@route('/<filename:re:.*\.(eot|ttf|woff|svg)>')
def fonts(filename):
    return static_file(filename, root='./static/fonts')

@route('/<filename:re:.*\.html>')
def send_static(filename):
    return static_file(filename, root='./')

@route('/')
def mainpage():
  return static_file('project_refined.html', root='.')

@route('/hello')
def hello():
  return "Hello World!"

@route('/_mix', method='GET')
@route('/_mix', method='POST')
#@route('/_mix')
def mix_model():
  postK = request.GET.get('K','').strip()
  postNoise = request.GET.get('noise','').strip()
  postHappy = request.GET.get('happy','').strip()
  postHalloween = request.GET.get('halloween','').strip()
  postBeer = request.GET.get('beer','').strip()
  postMusic = request.GET.get('music','').strip()
  postLunch = request.GET.get('lunch','').strip()
  happy = 1 if postHappy == 'true' else 0
  halloween = 1 if postHalloween == 'true' else 0
  beer = 1 if postBeer == 'true' else 0
  music = 1 if postMusic == 'true' else 0
  lunch = 1 if postLunch == 'true' else 0
  val = happy * pow(2, 4) + halloween * pow(2, 3) + beer * pow(2, 2) + music * pow(2, 1) + lunch * pow(2, 0)
  city = int(request.GET.get('cityind','').strip())
  if is_number(postK) and is_number(postNoise):
    k = int(postK)
    noise = float(postNoise)
    if k < 2 or k > 20:
      #return '<p>Please enter a number between 2 and 20 (inclusive) for the number of components.</p>'
      return {"result" : "Error"}
    if noise <= 0 or noise >= 1:
      #return '<p>Please enter a decimal between 0 and 1 (exclusive) for the prior probability of noise.</p>'
      return {"result" : "Error"}
    #return '<p>Running the mixture model with %d components, %.4f prior probability of noise, and words ?</p>' % (k, noise)

    return {"result" : cities[city] + '_k_' + str(k) + '_wc_' + str(val) + '_fitted.js'}
  else:
    #return '<p>Please enter valid numbers for the model tuning parameters.</p>'
    return {"result" : "Error"}

run(host='0.0.0.0', port=8888, debug=True)
