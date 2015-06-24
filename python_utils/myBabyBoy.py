import string
from datetime import date, timedelta

class Baby:
    def __init__(self, name, birthday, sex, mother, father):
        self.name = name 
        self.birthday = birthday 
        self.sex = sex
        self.mother = mother
        self.father = father
 
    def age(self):
        birth_date = date(year=int(self.birthday[0:4])\
                         ,month=int(self.birthday[4:6])\
                         ,day=int(self.birthday[6:8])) 
        today = date.today()
        return (today-birth_date).days

    def get_info(self):
        print 'Name:', self.name
        print 'Sex: ', self.sex
        print 'Age: ', self.age(), 'days'   

    def get_mother(self):
        return(self.mother)

    def happy_fathers_day(self):
        return 'Happy Father\'s Day, Daddy!'

def main():
    myBabyBoy = Baby('Aiden Li','20150616','Male','Congcong Li','Baolei Li')
    print '******************************'
    print '*',myBabyBoy.happy_fathers_day(),'*'
    print '--'
    print 'Mother:',myBabyBoy.get_mother()
    myBabyBoy.get_info() 
    print '******************************'
    
if __name__ == "__main__":
    main()
