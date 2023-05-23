# pylint: skip-file
import sys
import time
import os
from flask import Flask, render_template, request
sys.path.append('')
from Parser_GUI import parse_gui
app = Flask(__name__)


@app.route('/', methods=['GET'])
def home_page():
    '''
    The browser will render home.html when it visits '/' (the root of the web app)
    '''
    return render_template('home.html')


@app.route('/compile', methods=['GET'])
def compiles_page():
    '''
    The browser will render compile.html when it visits '/compile'
    '''
    return render_template('compile.html')


@app.route('/compile', methods=['POST'])
def compile_page():
    '''
    Defines what the browser should do when a post request (e.g. upload) is done on /compile 
    '''
    if request.method == 'POST':
        messages_arr=parse_gui(request.form['code'])
        print(messages_arr)
        #messages_arr="<br>".join(messages_arr)
        return render_template('compile.html', err_msg='', show_img=False, messages=messages_arr, code=request.form['code'])


if __name__ == '__main__':
    app.run(debug=True)
