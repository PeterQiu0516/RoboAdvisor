from flask_wtf import Form
from wtforms import StringField
from wtforms.validators import DataRequired

# class MyForm(Form):
#     user = StringField('Username', validators=[DataRequired()])

from flask import Flask, render_template,flash,request

app = Flask(__name__)
app.secret_key = '1234567'

# @app.route('/login', methods=('GET', 'POST'))
# def login():
#     form = MyForm()
#     if form.validate_on_submit():
#         # if form.user.data == 'admin':
#         if form.data['user'] == 'admin':
#             return 'Admin login successfully!'
#         else:
#             return 'Wrong user!'
#     return render_template('login.html', form=form)


from wtforms.fields import (StringField, PasswordField, DateField, BooleanField,
                            SelectField, SelectMultipleField, TextAreaField,
                            RadioField, IntegerField, DecimalField, SubmitField)
from wtforms.validators import DataRequired, Length, Email, EqualTo, NumberRange

class RegisterForm(Form):
    q1 = RadioField('1. 您的性别', choices=[('1', '男'), ('2', '女')],
                        validators=[DataRequired()])
    q2 = RadioField('2. 您的年龄', choices=[('1', '20-30'), ('2', '30-40'),('3', '40-50'),('4', '50-60'),
                                        ('5', '60+')],validators=[DataRequired()])
    q3 = RadioField('3. 您本次投资资金占家庭总资产比例(单选)', choices=[('1', '20-30'), ('2', '30-40'),('3', '40-50'),('4', '50-60'),
                                        ('5', '60+')],validators=[DataRequired()])
    q4 = RadioField('4. 您本次投资资金占家庭可投资资产比例(单选)', choices=[('1', '20-30'), ('2', '30-40'),('3', '40-50'),('4', '50-60'),
                                        ('5', '60+')],validators=[DataRequired()])
    q5 = RadioField('5. 您可以承受的最大投资亏损(单选)', choices=[('1%', '小于5%'), ('2', '5%-10%'),
                                        ('3', '10%-20%'), ('4', '>20%'), ], validators=[DataRequired()])
    q6 = RadioField('6. 您本次投资是为了(单选)', choices=[('1', '获得比银行存款更高的收益'),
                                                ('2', '教育储备'), ('3', '养老储备'), ('4', '资产增值'),],
                    validators=[DataRequired()])
    q7 = RadioField('7. 您本次投资愿意承担(单选)', choices=[('1', '高风险高收益'), ('2', '适中风险稳健收益'),
                                                 ('3', '低风险低收益')], validators=[DataRequired()])


    # Select类型，下拉单选框，choices里的内容会在Option里，里面每个项是(值，显示名)对
    q8 = SelectMultipleField('8. 您过往投资过的产品(多选)', choices=[
        ('1', '银行理财'),
        ('2', '股票'),
        ('3', '债券'),
        ('4', '保险'),
        ('5', '衍生品')
    ])

    # Select类型，多选框，choices里的内容会在Option里，里面每个项是(值，显示名)对
    q9 = SelectMultipleField('9. 您不愿意投资的基金类别:(多选)', choices=[
        ('1', '股票基金 – 综合类型 大型公司 中小型公司 行业主题 '),
        ('2', '债券基金 – 政府债券 公司债券 可转换债券'),
        ('3', '混合型基金 – 股债混合型基金'),
        ('4', '货币市场基金')
    ])

    q10 = SelectMultipleField('10. 您不愿意投资的地区：(多选)', choices=[
        ('1 ', '非洲及中东'),
        ('2', '亚太地区'),
        ('3', '金砖四国'),
        ('4', '新兴市场'),
        ('5', '欧洲'),
        ('6', '环球'),
        ('7', '大中华'),
        ('8', '北美')
    ])

    # Submit按钮
    submit = SubmitField('Submit')


@app.route('/register', methods=('GET', 'POST'))
def register():
    form = RegisterForm()

    s_option = request.values.getlist("s_option")
    for s in s_option:
        return render_template('hello2.html',op=s_option)
    if form.validate_on_submit():
        # flash('"%s" ' % form.q1.data)
        # login_form = LoginForm()
        # return render_template('login.html', form=login_form)
        # a=[]
        # a.append(form.q1.data)
        # a.append(form.q2.data)
        # a.append(form.q3.data)
        # a.append(form.q4.data)
        # a.append(form.q5.data)
        # b=[1,2,3,4,5]
        from rpy2 import robjects
        # 载入导入包函数
        from rpy2.robjects.packages import importr
        # 将stats包导入为模块
        stats = importr('stats')
        base = importr('base')
        robjects.r('''
                   f <- function(r){pi * r}
                   ''')
        robjects.r['f'](form.q1.data)


        return render_template('hello.html',name=form.q1.data)

    return render_template('register.html', form=form)

if __name__ == '__main__':
    app.run(debug = True)