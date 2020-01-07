
# from flask import Flask, render_template, request,redirect,url_for
# app = Flask(__name__)

# @app.route('/')
# def index():
#     return render_template('index.html')
#
#
# @app.route('/search')
# def search():
#     print (request.args) #get请求，打印url后面所有的参数（key:value形式），如果有多个参数，通过request.args.get('key')的方式获取值
#     return render_template('search.html')
#
#
# @app.route('/login/',methods=['POST','GET'])
# def login():
#     if request.method == 'GET': #此处判断get和post方法与django相同
#         return render_template('login.html')
#
#     else:
#         username = request.form.get('username') #post请求。获取模版语言中输入框输入的值
#         password = request.form.get('password')
#
#         return "post request, username: %s password:%s" % (username,password)


from flask import Flask
from flask_sqlalchemy import SQLAlchemy

app = Flask(__name__)

# 设置连接数据库的URL
app.config['SQLALCHEMY_DATABASE_URI'] = 'mysql://root:mysql@127.0.0.1:3306/Flask_test'

# 设置每次请求结束后会自动提交数据库中的改动
app.config['SQLALCHEMY_COMMIT_ON_TEARDOWN'] = True

app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = True
# 查询时会显示原始SQL语句
app.config['SQLALCHEMY_ECHO'] = True
db = SQLAlchemy(app)


class Role(db.Model):
    # 定义表名
    __tablename__ = 'roles'
    # 定义列对象
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(64), unique=True)
    us = db.relationship('User', backref='role')

    # repr()方法显示一个可读字符串
    def __repr__(self):
        return 'Role:%s' % self.name


class User(db.Model):
    __tablename__ = 'users'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(64), unique=True, index=True)
    email = db.Column(db.String(64), unique=True)
    pswd = db.Column(db.String(64))
    role_id = db.Column(db.Integer, db.ForeignKey('roles.id'))

    def __repr__(self):
        return 'User:%s' % self.name


if __name__ == '__main__':
    db.drop_all()
    db.create_all()
    ro1 = Role(name='admin')
    ro2 = Role(name='user')
    db.session.add_all([ro1, ro2])
    db.session.commit()
    us1 = User(name='wang', email='wang@163.com', pswd='123456', role_id=ro1.id)
    us2 = User(name='zhang', email='zhang@189.com', pswd='201512', role_id=ro2.id)
    us3 = User(name='chen', email='chen@126.com', pswd='987654', role_id=ro2.id)
    us4 = User(name='zhou', email='zhou@163.com', pswd='456789', role_id=ro1.id)
    db.session.add_all([us1, us2, us3, us4])
    db.session.commit()
    app.run(debug=True)









