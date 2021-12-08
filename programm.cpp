#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <stdio.h>
#include <cstdlib>
#include <ctime>
#include <chrono>
#include <cmath>
using namespace std;

//сортировка строк
void swap_rows(double** a, int r1, int r2)
{
    double* b = a[r1];
    a[r1] = a[r2];
    a[r2] = b;
}

//Факториал 
long double fact(int N)
{
    if (N < 0) 
        return 0; 
    if (N == 0) 
        return 1; 
    else 
    
    return N * fact(N - 1);
}

//Евклидово расстояние двух точек 
double EuclideanDistance(double* x, double* y, int columnNumber)
{
    double diff = 0;
    double d = 0;

    for (int i = 0; i < columnNumber; i++)
    {
        diff = 0;
        diff = (x[i] - y[i])*(x[i] - y[i]);
        d = d + diff;
    }

    return sqrt(d);
}

//задание не целого случайного числа из промежутка
double xrand(double min, double max)
{
	return min + double(rand()) / double(RAND_MAX) * (max - min);
}

//случайная перемешивание массивов с данными и классами
void shuffle(double** arr, double* arr2, int N, int C)//arr,arr2 - массивы для перестановки, N - количество строк, С - количество столбцов
{
    // инициализация генератора случайных чисел
    srand(time(NULL));
 
    // реализация алгоритма перестановки
    for (int i = N - 1; i >= 1; i--)
    {
        int j = rand() % (i + 1);
 
        double tmp2 = arr2[j];
        arr2[j] = arr2[i];
        arr2[i] = tmp2;

        for (int g = 0; g < C; g++)
        {
            double tmp = arr[j][g];
            arr[j][g] = arr[i][g];
            arr[i][g] = tmp;
        }
    }
}

//УНИВЕРСАЛЬНЫЙ ТЕРМ
double term_universal(double x, int current_term)
{
    double t = 0;
    int term = 14;

    if (current_term > 0 && current_term < 3)
        term = 2;
    else if (current_term > 2 && current_term < 6)
    {
        term = 3;
        current_term = current_term - 2;
    }
    else if (current_term > 5 && current_term < 10)
    {
        term = 4;
        current_term = current_term - 5;
    }
    else if (current_term > 9 && current_term < 15)
    {
        term = 5;
        current_term = current_term - 9;
    }

    double v1 = current_term / (term - 1.0);

    if (current_term == term && x > 1)
        return 1;
    else if (current_term == 1 && x < 0)
        return 1;
    else if (x < (current_term / (term - 1.0)))
        t = x * (term - 1) - current_term + 2;
    else
        t = -x * (term - 1) + current_term;
    

    t = max(min(t, 1.0), 0.0);
    return t;
}

//создание правила
void create_rule(int num_term, int columnNumber, double** x, int* rule, int* random_objects, int ran)
{
    double* vero_fit = new double[num_term];
    double** m_allterm = new double*[num_term];
    for (int i = 0; i < num_term; i++)
    {
        m_allterm[i] = new double[columnNumber];
    }

    for (int i = 0; i < ran; i++)
    {
        //проходим по всем его переменным
        for (int l = 0; l < num_term; l++)
        {
            //для каждой переменной смотрим под какие нечеткие термы эта переменная подходит и с какой степенью
            for (int j = 0; j < columnNumber; j++)
            {
                //запоминаем все степени принадлежности переменной в массив
                m_allterm[l][j] = term_universal(x[random_objects[i]][j], l + 1);
            }
        }
    }

    for (int l = 0; l < columnNumber; l++)
    {
        //пропорциональная селекция
        double sum_fit = 0;

        for (int j = 0; j < num_term; j++)
        {
            sum_fit += m_allterm[j][l]/ran;
        }

        for (int j = 0; j < num_term; j++)
        {
            if (sum_fit != 0)
            {
                vero_fit[j] = m_allterm[j][l]/ran / sum_fit;
            }
            else
            {
                vero_fit[j] = 1.0 / (double)num_term;
            }
        }

        double sum_interval = 0;
        double prand = xrand(1, 0);
        for (int j = 0; j < num_term; j++)
        {
            if (sum_interval < prand && prand < (sum_interval + vero_fit[j]))
            {
                rule[l] = j + 1;
            }
            sum_interval += vero_fit[j];
        }
    }
    //с вероятностью > 0.85 заменяем на don't care 
    for (int j = 0; j < columnNumber; j++)
    {
        double change = xrand(1, 0);
        if (change > 0.85)
        {
            rule[j] = 0;
        }
    }

    for (int i = 0; i < num_term; i++)
    {
        delete m_allterm[i];
    }
    delete[] m_allterm;
    delete[] vero_fit;
}

//Подаем правило, выводим мю для такого-то класса
double check_conf_rule(double* x, int* rule_gen, int col)
{
    double max = 0;
    double min = 1;
    for (int b = 0; b < col; b++)
    {
        double temp = 0;
        if (rule_gen[b] == 0)
        {
            temp = 1;
        }
        else
        {
            temp = term_universal(x[b], rule_gen[b]);
        }

        if (temp < min)
        {
            min = temp;
        }
    }
    //Процедура argmax
    if (min > max)
    {
        max = min;
    }
    
    return max;
}

//confidence measure of the fuzzy association rule
void confidence(int num_class, int lineNumber, int columnNumber, double* class_answers, double** data, int* rule, double* confid)
{
    //расчитываем конфиденс
    double* m = new double[num_class];
    double sum_m = 0;
    for (int j = 0; j < lineNumber; j++)
    {
        for (int l = 0; l < num_class; l++)
        {
            if (class_answers[j] == l)
            {
                m[l] += check_conf_rule(data[j], rule, columnNumber);
            }
        }
        sum_m += check_conf_rule(data[j], rule, columnNumber);
    }

    for (int j = 0; j < num_class; j++)
    {
        confid[j] = m[j] / sum_m;
    }
    delete[] m;
}

//База правил подаем координаты, возвращает номер класс
void Rules(double* x, int** rules_gen, int* class_rule, int* active_rule, int col, int num_rules, int** best_rule_for_object, double** reply, int y, int j)
{
    double max = 0;
    int number_rule_best = 0;
    int checki = 0;
    for (int p = 0; p < num_rules; p++)
    {
        if (active_rule[p] == 1)//если правило активно, то считаем
        {
            double min = 1;
            for (int b = 0; b < col; b++)
            {
                double temp = 0;
                if (rules_gen[p][b] == 0)
                {
                    temp = 1;
                }
                else
                {
                    temp = term_universal(x[b], rules_gen[p][b]);
                }

                if (temp < min)
                {
                    min = temp;
                }
            }
            //Процедура argmax
            if (min > max)
            {
                max = min;
                number_rule_best = p;
            }
        }
    }
    best_rule_for_object[y][j] = number_rule_best;
    checki = class_rule[number_rule_best];
    reply[y][j] = class_rule[number_rule_best];
}

//Количество верно классифицированных объектов одним правилом
void check_fitness_michegan(int* correct_classification_for_object_train, int* best_rule_for_object_train, int y, int num_rules, int linenumber, int** fitness_michegan)
{
    for (int i = 0; i < linenumber; i++)
    {
        for (int j = 0; j < num_rules; j++)
        {
            if ((best_rule_for_object_train[i] == j) && (correct_classification_for_object_train[i] == 1))
            {
                fitness_michegan[y][j] = fitness_michegan[y][j] + 1;
            }
        }
    }
}

//подсчет количества активных правил в популяции
int active_rule_flag(int* active_rules, int number_rules)
{
    int flag = 0;
    for (int l = 0; l < number_rules; l++)
    {
        if (active_rules[l] == 1)
        {
            flag++;
        }
    }
    return flag;
}

//don't care среднее в популяции по правиле
int dont_care_flag(int number_rules, int columnNumber, int** x)
{
    int flag = 0;
    int average = 0;
    for (int i = 0; i < number_rules; i++)
    {
        flag = 0;
        for (int l = 0; l < columnNumber; l++)
        {
            if (x[i][l] != 0)
            {
                flag++;
            }
        }
        average = average +  flag / columnNumber;
    }
    average = average / number_rules;
    return average;
}

//матрица ошибок для n классов
double error_matrix(int train_length, double* reply_train, double* train_class_answers, double* class_value, int num_class, int* class_values_count_train)
{
    //подсчет количества каждого класса в массиве 
    int* class_values_count_reply = new int[num_class - 1]; 

    //подсчет количества повторяющихся значений для каждого класса в массиве
    for(int class_id = 0; class_id < num_class; class_id++)
    {
        int n = 0;
        for(int j = 0; j < train_length; j++)
        {		
            if(reply_train[j]==class_value[class_id]) 
            {
                n++;
            }	
        }
        class_values_count_reply[class_id] = n;	      
    } 
    
    double* mtrue = new double[num_class];
    double** mfalse = new double* [num_class]; 
    for (int i = 0; i < num_class; i++)
    {
        mfalse[i] = new double[num_class];
    }

    for(int class_id = 0; class_id < num_class; class_id++)
    {
        for (int i = 0; i < train_length; i++)
        {
            if (reply_train[i] == train_class_answers[i])
            {
                mtrue[class_id] =+ 1;
            }
            else
            {
                mfalse[int(train_class_answers[i])][int(reply_train[i])] =+ 1;
            }
        }
    }
    
    for (int i = 0; i < num_class; i++)
    {
        delete mfalse[i];
    }
    delete mfalse;
    delete[] mtrue;
    delete[] class_values_count_reply;
    return 0;
}

int main () {
    srand(time(0));//в самом начале один раз для рандома
    chrono::steady_clock sc;   // создание объекта `steady_clock` class
    auto start = sc.now();     // старт таймера
//--------------------------------------работа с файлом-------------------------------------------------------------------
    //получение кол-ва строк и столбов
    //запись данных в массив
    string line, csvItem;
    string read_file;
    cout << "Please entry file name and his extension (for example, iris.txt) - ";
    cin >> read_file;

    ifstream myfile (read_file.c_str());//считываение файла
    int lineNumber = 0;//кол-во строк
    int columnNumberFile = 0;//количество столбцов во всем файле(параметры + класс)

    if (myfile.is_open()) 
    {
        while (getline(myfile,line)) 
        {
            lineNumber++;
            istringstream myline(line);
            int get_column = 0;
            while(getline(myline, csvItem, ' ')) 
            {
                get_column++;
            }
            columnNumberFile = get_column;
        }
        myfile.close();
    }
    //инициализация массива с данными
    double** file_mas = new double*[lineNumber];
    for (int i = 0; i < lineNumber; i++)
    {
        file_mas[i] = new double[columnNumberFile];
    }
    
    ifstream myfile2 (read_file.c_str());//открываем второй раз для получения данных

    if (myfile2.is_open()) 
    {
        for (int i = 0; i < lineNumber; i++)
	    {
            for (int j = 0; j < columnNumberFile; j++)
	        {
                myfile2 >> file_mas[i][j];
            }
	    }
        /*
        //вывод в терминал массива
        for (int i = 0; i < lineNumber; i++)
	    {
            for (int j = 0; j < columnNumberFile; j++)
	        {
                cout << file_mas[i][j] << " ";
            }
            cout << endl;
	    }*/
        myfile2.close();
    }
    //запись в массив классов и объектов
    int columnNumber = columnNumberFile - 1;//количество столбцов для параметров
    //массив, в котором лежат классы для каждого объекта 
    double* class_answers = new double[lineNumber];
    for (int i = 0; i < lineNumber; i++)
    {
        class_answers[i] = file_mas[i][columnNumber];
    }
    //массива с объектами без классов
    double** data_file = new double*[lineNumber];
    for (int i = 0; i < lineNumber; i++)
    {
        data_file[i] = new double[columnNumber];
    }

    for (int i = 0; i < lineNumber; i++)
    {
        for (int j = 0; j < columnNumber; j++)
        {
            data_file[i][j] = file_mas[i][j];
        }
    }
    //вывод в терминал массивов
    /*for (int i = 0; i < lineNumber; i++)
    {
        for (int j = 0; j < columnNumber; j++)
        {
            cout << data_file[i][j] << " ";
        }
        cout << endl;
    }
    
    for (int i = 0; i < lineNumber; i++)
    {
        cout << class_answers[i] << " ";
    }*/
    //операции с массивом классов (подсчет кол-ва классов и кол-ва объектов конкретного класса)
    int num_class = 0;//количество значений классов
    //подсчет количества уникальных значений в массиве классов 
    for (int i = 0; i < lineNumber; ++i)
    {
        int j;
        for (j = i + 1; j < lineNumber && class_answers[j] != class_answers[i]; ++j);
        num_class += j == lineNumber;      
    }
    //std::cout << "Number of classes " << num_class << std::endl;

    double* class_value = new double[num_class - 1];
    int* class_values_count = new int[num_class - 1];

    //подсчет количества повторяющихся значений для каждого класса
    int class_count = 0;
    for(int i = 0; i < lineNumber; i++ ){
		int n = 0;
		for(int j = 0; j < lineNumber; j++){
			if(class_answers[i]==class_answers[j]) 
            {
				if(i > j)	break;
				n++;
			}					
		}
		//if(n) std::cout << class_answers[i] << " " << n << " times\n";
        if(n)
        {
            class_value[class_count] = class_answers[i];
            class_values_count[class_count] = n;
            class_count++;
        }	
	}
    
    //вывод повторяющихся значений класса
    /*for (int i = 0; i < num_class; i++)
    {
        cout << class_value[i] << " ";
        cout << class_values_count[i] << " ";
        cout << "\n";
    }*/
//-------------------------------------------ключевые параметры---------------------------------------------------
    //кол-во индивидов
    int pop_size = 100;//потом спросить у пользователя
    //кол-во поколений
    int gen = 1000;//потом спросить у пользователя
    int max_gen = gen;
    //количество правил
    int number_rules = 50;
    //турнирная селекция
    int T = 4;
    int w1 = 100;
    int w2 = 1;
    int w3 = 1;
    int npop = 4;
    //количество рандомных объектов, по которым провести турнирную селекцию для формирования правила
    int num_random = 4;
    //количество объектов, на основании которых создается правило
    int num_obj_create_rule = 2;
    int kfold = 10;
    int cross_num = lineNumber / kfold;
    int last_data = lineNumber % kfold;
    int cross_num_const = cross_num;
    int better_than = 0.5;//параметр для прохождения confidence
    int which_initial = 2;
    //0 - формирование правила с одного случайного объекта
    //1 - с n случайных объектов
    //2 - с n случайных объектов с минимальным евклидовым расстоянием

//-----------------------------------------------нормировка------------------------------------------------------
    double** data = new double*[lineNumber];
    for (int i = 0; i < lineNumber; i++)
    {
        data[i] = new double[columnNumber];
    }

    double* max = new double[columnNumber];
    double* min = new double[columnNumber];

    for (int i = 0; i < columnNumber; i++)//поиск для каждого параметра max элемент
    {
        max[i] = data_file[0][i];
        for (int j = 1; j < lineNumber; j++)
        {
            if (data_file[j][i] > max[i])
            {
                max[i] = data_file[j][i];
            }
        }
    }

    for (int i = 0; i < columnNumber; i++)//поиск для каждого параметра min элемент
    {
        min[i] = data_file[0][i];
        for (int j = 1; j < lineNumber; j++)
        {
            if (data_file[j][i] < min[i])
            {
                min[i] = data_file[j][i];
            }
        }
    }

    //нормировка
    for (int i = 0; i < columnNumber; i++)
    {
        for (int j = 0; j < lineNumber; j++)
        {
            if (max[i] == min[i])
            {
                data[j][i] = 0;
            }
            else
            {
                data[j][i] = (data_file[j][i] - min[i]) / (max[i] - min[i]);//новая матрица со значениями от 0 до 1
            }
        }
    }

    /*//вывод в терминал массива нормировки
    for (int i = 0; i < lineNumber; i++)
    {
        for (int j = 0; j < columnNumber; j++)
        {
            cout << data[i][j] << " ";
        }
        cout << endl;
    }*/
//---------------------------------------обучающая и тестовая выборки---------------------------------------------    
    //случайное перемешивание массивов с данными и с ответами соответственно
    shuffle(data, class_answers, lineNumber, columnNumber);

    //вывод в терминал перемешанных массивов
    /*for (int i = 0; i < lineNumber; i++)
    {
        for (int j = 0; j < columnNumber; j++)
        {
            cout << data[i][j] << " ";
        }
        cout << endl;
    }
    
    for (int i = 0; i < lineNumber; i++)
    {
        cout << class_answers[i] << " ";
    }
    //cout << cross_num;*/
    
    int piece = 10;
    /*double** count_average_kfold = new double*[npop];
    for (int i = 0; i < npop; i++)
    {
        count_average_kfold[i] = new double[kfold];
    }
    //--------------------изменения-------------------------
    double average_fitness_kfold = 0;//точность на обучающей
    double average_fitness_kfold_test = 0;//точность на тестовой 
    double average_active_kfold = 0;//количество активных правил
    double average_accuracy_kfold = 0;//accuracy правила*/
    cross_num = cross_num_const;
    int train_length = lineNumber-(cross_num_const+last_data);
    int test_length = cross_num_const+last_data; 
    double** test_data = new double*[cross_num_const+last_data];
    for (int i = 0; i < cross_num_const+last_data; i++)
    {
        test_data[i] = new double[columnNumber];
    }
    double** train_data = new double*[lineNumber-(cross_num_const+last_data)];
    for (int i = 0; i < lineNumber-(cross_num_const+last_data); i++)
    {
        train_data[i] = new double[columnNumber];
    }
    double* test_class_answers = new double[cross_num_const+last_data];
    double* train_class_answers = new double[lineNumber-(cross_num_const+last_data)];

    for (int i = 0; i < kfold; i++)
    {
        cout << "Piece " << i << endl;

        //0 - pop
        //1 - pop2
        //2 - pop3
        int num_term = 14;    
        double*** confid_rules = new double** [npop];
        double*** weight_rules = new double** [npop];
        int*** active_rules = new int** [npop];
        int*** class_rules = new int** [npop];
        int*** rules_update = new int** [npop];

        for (int ip = 0; ip < npop; ip++)
        {
            confid_rules[ip] = new double*[pop_size];
            weight_rules[ip] = new double*[pop_size];
            active_rules[ip] = new int*[pop_size];
            class_rules[ip] = new int*[pop_size];
            rules_update[ip] = new int*[pop_size];
        
            for (int j = 0; j < pop_size; j++)
            {
                confid_rules[ip][j] = new double[number_rules];
                weight_rules[ip][j] = new double[number_rules];
                active_rules[ip][j] = new int[number_rules];
                class_rules[ip][j] = new int[number_rules];
                rules_update[ip][j] = new int[number_rules];
            }
        }
        for (int ip = 0; ip < npop; ip++)
        {
            for (int j = 0; j < pop_size; j++)
            {
                for (int l = 0; l < number_rules; l++)
                {
                    active_rules[ip][j][l] = 0;
                }
            }
        }

        int*** pop = new int** [pop_size];
        int*** pop2 = new int** [pop_size];
        int*** pop3 = new int** [pop_size];
        int*** out = new int** [pop_size];
        int** best_rule_for_object_train = new int*[pop_size];
        int** best_rule_for_object_test = new int*[pop_size];
        int** correct_classification_for_object_train = new int*[pop_size];
        int** fitness_michegan = new int*[pop_size];
        int** correct_classification_num = new int*[pop_size];
        double* fitness = new double[pop_size];
        double* fitness_small = new double[pop_size];

        for (int ip = 0; ip < pop_size; ip++)
        {
            pop[ip] = new int*[number_rules];
            pop2[ip] = new int*[number_rules];
            pop3[ip] = new int*[number_rules];
            out[ip] = new int*[number_rules];
            best_rule_for_object_train[ip] = new int[lineNumber-(cross_num_const+last_data)];
            best_rule_for_object_test[ip] = new int[cross_num_const+last_data];
            fitness_michegan[ip] = new int[number_rules];
            correct_classification_num[ip] = new int[number_rules];
            //правильно классифицированный объект - 1
            //неправильно классифицированный объект - 0
            correct_classification_for_object_train[ip] = new int[lineNumber-(cross_num_const+last_data)];

            for (int j = 0; j < number_rules; j++)
            {
                pop[ip][j] = new int[columnNumber];
                pop2[ip][j] = new int[columnNumber];
                pop3[ip][j] = new int[columnNumber];
                out[ip][j] = new int[columnNumber];
            }
        }
        for (int ip = 0; ip < pop_size; ip++)
        {
            for (int j = 0; j < number_rules; j++)
            {
                for (int l = 0; l < columnNumber; l++)
                {
                    pop[ip][j][l] = 0;
                    pop2[ip][j][l] = 0;
                    pop3[ip][j][l] = 0;
                    out[ip][j][l] = 0;
                }
            }
        }
        for (int ip = 0; ip < pop_size; ip++)
        {
            for (int j = 0; j < lineNumber-(cross_num_const+last_data); j++)
            {
                best_rule_for_object_train[ip][j] = 0;
            }
            for (int j = 0; j < number_rules; j++)
            {
                fitness_michegan[ip][j] = 0;
                correct_classification_num[ip][j] = 0;
            }
        }

        if (i == 0)
        {
            //тестовые данные
            for (int j = 0; j < cross_num+last_data; j++)
            {
                for (int g = 0; g < columnNumber; g++)
                {
                    test_data[j][g] = data[j][g];
                    test_class_answers[j] = class_answers[j];
                }
            } 
            //обучающие даные
            int counter = 0;
            for (int j = cross_num+last_data; j < lineNumber; j++)
            {
                for (int g = 0; g < columnNumber; g++)
                {
                    train_data[counter][g] = data[j][g];
                    train_class_answers[counter] = class_answers[j];
                }
                counter++;
            } 
        }
        else if (i != 0 && i != kfold-1)
        {
            //тестовые данные
            int counter = 0;
            for (int j = cross_num; j < cross_num+cross_num_const+last_data; j++)
            {
                for (int g = 0; g < columnNumber; g++)
                {
                    test_data[counter][g] = data[j][g];//здесь
                    test_class_answers[counter] = class_answers[j];
                }
                counter++;
            } 
            //обучающие даные
            for (int j = 0; j < cross_num; j++)
            {
                for (int g = 0; g < columnNumber; g++)
                {
                    train_data[j][g] = data[j][g];
                    train_class_answers[j] = class_answers[j];
                }
            } 
            counter = 0;
            for (int j = cross_num+cross_num_const+last_data; j < lineNumber; j++)
            {
                for (int g = 0; g < columnNumber; g++)
                {
                    train_data[counter][g] = data[j][g];
                    train_class_answers[counter] = class_answers[j];
                }
                counter++;
            } 
            cross_num = cross_num + cross_num_const;
        }
        else if (i == kfold-1)
        {
            //тестовые данные
            int counter = 0;
            for (int j = lineNumber-(cross_num_const+last_data); j < lineNumber; j++)
            {
                for (int g = 0; g < columnNumber; g++)
                {
                    test_data[counter][g] = data[j][g];
                    test_class_answers[counter] = class_answers[j];
                }
                counter++;
            } 
            //обучающие даные
            for (int j = 0; j < lineNumber-(cross_num_const+last_data); j++)
            {
                for (int g = 0; g < columnNumber; g++)
                {
                    train_data[j][g] = data[j][g];
                    train_class_answers[j] = class_answers[j];
                }
            } 
        }
        int check_gen = 0;

        //подсчет количества каждого класса в train выборке!!!
        int* class_values_count_train = new int[num_class - 1];

        //задача подбора информативных признаков 

        //подсчет количества повторяющихся значений для каждого класса в train set
        for(int class_id = 0; class_id < num_class; class_id++)
        {
            int n = 0;
            for(int j = 0; j < train_length; j++)
            {
                if(train_class_answers[j]==class_value[class_id]) 
                {
                    n++;
                }					
            }
            class_values_count_train[class_id] = n;	
            
        } 
        
        //вывод повторяющихся значений класса для train set
        /*for (int m = 0; m < num_class; m++)
        {
            cout << class_value[m] << " ";
            cout << class_values_count_train[m] << " ";
            cout << "\n";
        }*/

        /*for (int i = 0; i < lineNumber-(cross_num+last_data); i++)
        {
            for (int j = 0; j < columnNumber; j++)
            {
                cout << train_data[i][j] << " ";//120
            }
            cout << endl;
        }
        break;*/
        
        /*for (int i = 0; i < cross_num+last_data; i++)
        {
            for (int j = 0; j < columnNumber; j++)
            {
                cout << test_data[i][j] << " ";//30
            }
            cout << endl;
        }
        break;*/
//-------------------------------------------начало га-лгоритма----------------------------------------------------
        //инициализация
        int q_number = 0;
        if (which_initial == 0)
        {
            for (int ipop = 0; ipop < pop_size; ipop++)
            {
                //cout << i << " ";
                //cout << endl;
                if (columnNumber <= number_rules/2)
                {
                    q_number = columnNumber;
                }
                else
                {
                    q_number = number_rules/2;
                }
                
                int flag = 0;
                for (int q = 0; q < q_number; q++)
                {
                    int* in_rule = new int[columnNumber];
                    for (int j = 0; j < columnNumber; j++)
                    {
                        in_rule[j] = 0;
                    }
                    //случайный объект из выборки
                    int random_obj = rand() % train_length;
                    int* obj_for_rule = new int[1];
                    num_obj_create_rule = 1;
                    obj_for_rule[0] = random_obj;
                    create_rule(num_term, columnNumber, train_data, in_rule, obj_for_rule, num_obj_create_rule);

                    /*for (int j = 0; j < columnNumber; j++)
                    {
                        cout << rule[j] << " ";
                    }*/

                    double* in_confid = new double[num_class];
                    for (int j = 0; j < num_class; j++)
                    {
                        in_confid[j] = 0;
                    }
                    
                    confidence(num_class, lineNumber, columnNumber, class_answers, data, in_rule, in_confid);
                    /*
                    for (int j = 0; j < num_class; j++)
                    {
                        cout << in_confid[j] << " ";
                    }*/

                    cout << endl << " Правило " << endl;
                    for (int j = 0; j < columnNumber; j++)
                    {
                        cout << in_rule[j] << " ";
                    }

                    double max_confid = 0;
                    int c_confid = 0;
                    for (int j = 0; j < num_class; j++)
                    {
                        if (in_confid[j] > max_confid)
                        {
                            max_confid = in_confid[j];
                            c_confid = j;
                        }
                    }

                    if (max_confid > better_than)
                    {
                        flag++;
                        for (int y = 0; y < columnNumber; y++)
                        {
                            pop[ipop][q][y] = in_rule[y];
                        }
                        class_rules[0][ipop][q] = c_confid;
                        confid_rules[0][ipop][q] = max_confid;
                        weight_rules[0][ipop][q] = 2*max_confid - 1;
                        active_rules[0][ipop][q] = 1;
                    }
                    else
                    {
                        for (int y = 0; y < columnNumber; y++)
                        {
                            pop[ipop][q][y] = 100;
                        }
                        active_rules[0][ipop][q] = 0;
                    }

                    delete[] in_rule;
                    delete[] in_confid;
                    delete[] obj_for_rule;
                }
                if (flag < num_class)
                {
                    //сделать по-умному
                    cout << endl << " МЕНЬШЕ МИНИМАЛЬНОГО " << endl;
                }
                
                /*for (int j = 0; j < number_rules; j++)
                {
                    for (int y = 0; y < columnNumber; y++)
                    {
                        cout << pop[i][j][y] << " ";
                    }
                    cout << endl;
                }*/
                
                /*for (int y = 0; y < number_rules; y++)
                {
                    cout << class_rules[0][i][y] << " ";
                    if (active_rules[0][i][y] == 0)
                    {
                        cout << "<-wrong ";
                    }
                }*/
                /*for (int y = 0; y < number_rules; y++)
                {
                    cout << "active " << active_rules[0][i][y] << endl;
                    cout << "confidence " << confid_rules[0][i][y] << endl;
                    cout << "weight " << weight_rules[0][i][y] << endl;
                }
                cout << endl;*/
            }
        }
        else if (which_initial == 1)
        {
            for (int ipop = 0; ipop < pop_size; ipop++)
            {
                //cout << i << " ";
                //cout << endl;
                if (columnNumber <= number_rules/2)
                {
                    q_number = columnNumber;
                }
                else
                {
                    q_number = number_rules/2;
                }
                
                int flag = 0;
                for (int q = 0; q < q_number; q++)
                {
                    int* in_rule = new int[columnNumber];
                    for (int j = 0; j < columnNumber; j++)
                    {
                        in_rule[j] = 0;
                    }
                    //случайный объект из выборки
                    int* obj_for_rule = new int[num_obj_create_rule];
                    for (int j = 0; j < num_obj_create_rule; j++)
                    {
                        obj_for_rule[j] = rand() % train_length;
                    }
                    create_rule(num_term, columnNumber, train_data, in_rule, obj_for_rule, num_obj_create_rule);

                    /*for (int j = 0; j < columnNumber; j++)
                    {
                        cout << rule[j] << " ";
                    }*/

                    double* in_confid = new double[num_class];
                    for (int j = 0; j < num_class; j++)
                    {
                        in_confid[j] = 0;
                    }
                    
                    confidence(num_class, lineNumber, columnNumber, class_answers, data, in_rule, in_confid);
                    /*
                    for (int j = 0; j < num_class; j++)
                    {
                        cout << in_confid[j] << " ";
                    }*/

                    cout << endl << " Правило " << endl;
                    for (int j = 0; j < columnNumber; j++)
                    {
                        cout << in_rule[j] << " ";
                    }

                    double max_confid = 0;
                    int c_confid = 0;
                    for (int j = 0; j < num_class; j++)
                    {
                        if (in_confid[j] > max_confid)
                        {
                            max_confid = in_confid[j];
                            c_confid = j;
                        }
                    }

                    if (max_confid > better_than)
                    {
                        flag++;
                        for (int y = 0; y < columnNumber; y++)
                        {
                            pop[ipop][q][y] = in_rule[y];
                        }
                        class_rules[0][ipop][q] = c_confid;
                        confid_rules[0][ipop][q] = max_confid;
                        weight_rules[0][ipop][q] = 2*max_confid - 1;
                        active_rules[0][ipop][q] = 1;
                    }
                    else
                    {
                        for (int y = 0; y < columnNumber; y++)
                        {
                            pop[ipop][q][y] = 100;
                        }
                        active_rules[0][ipop][q] = 0;
                    }

                    delete[] in_rule;
                    delete[] in_confid;
                    delete[] obj_for_rule;
                }
                if (flag < num_class)
                {
                    //сделать по-умному
                    cout << endl << " МЕНЬШЕ МИНИМАЛЬНОГО " << endl;
                }
                
                /*for (int j = 0; j < number_rules; j++)
                {
                    for (int y = 0; y < columnNumber; y++)
                    {
                        cout << pop[i][j][y] << " ";
                    }
                    cout << endl;
                }*/
                
                /*for (int y = 0; y < number_rules; y++)
                {
                    cout << class_rules[0][i][y] << " ";
                    if (active_rules[0][i][y] == 0)
                    {
                        cout << "<-wrong ";
                    }
                }*/
                /*for (int y = 0; y < number_rules; y++)
                {
                    cout << "active " << active_rules[0][i][y] << endl;
                    cout << "confidence " << confid_rules[0][i][y] << endl;
                    cout << "weight " << weight_rules[0][i][y] << endl;
                }
                cout << endl;*/
            }
        }
        else 
        {
            for (int ipop = 0; ipop < pop_size; ipop++)
            {
                //задание изначального количества для популяции
                if (columnNumber <= number_rules/2)
                {
                    q_number = columnNumber;
                }
                else
                {
                    q_number = number_rules/2;
                }
                    
                int flag = 0;
                int* random_object = new int[num_random];
                int* obj_for_rule = new int[num_obj_create_rule];
                for (int j = 0; j < num_obj_create_rule; j++)
                {
                    obj_for_rule[j] = -1;
                }
                //заполнение правил
                for (int q = 0; q < q_number; q++)
                {
                    int* in_rule = new int[columnNumber];
                    for (int j = 0; j < columnNumber; j++)
                    {
                        in_rule[j] = 0;
                    }
                    //случайный объект из выборки
                    int random_obj = rand() % train_length;
                    //массив с num_random случайными объектами из выборки одного класса
                    random_object[0] = rand() % train_length;
                    int answer_class_check = train_class_answers[random_object[0]];
                    int count = 1;
                    for (int ran = 1; ran < num_random; ran++)
                    {
                        random_obj = rand() % train_length;
                        while (train_class_answers[random_obj] != train_class_answers[random_object[0]])
                        {
                            random_obj = rand() % train_length;
                        }
                        int check_same = 0;
                        for (int j = 0; j < count; j++)
                        {
                            if (random_obj == random_object[j])
                            {
                                check_same = 1;
                            }
                        }
                        if (check_same == 0)
                        {
                            random_object[ran] = random_obj;
                            count++;
                        }
                        else 
                        {
                            random_obj = rand() % train_length;
                            while ((train_class_answers[random_obj] != train_class_answers[random_object[0]]))
                            {
                                random_obj = rand() % train_length;
                                count++;
                            }
                            random_object[ran] = random_obj;
                        }
                    }

                    /*for (int j = 0; j < num_random; j++)
                    {
                        cout << random_object[j] << " ";
                    }*/

                    int ed_fact = 0;
                    ed_fact = num_random - 1;
                    double** edist = new double*[ed_fact];
                    for (int e = 0; e < ed_fact; e++)
                    {
                        edist[e] = new double[2];
                    }

                    for (int j = 0; j < ed_fact; j++)
                    {
                        for (int j1 = 0; j1 < 2; j1++)
                        {
                            edist[j][j1] = 0;
                        }
                    }

                    //обработка массива с выбранными данными из одного класса
                    int dcount = 0;
                    int edist_min = 100;
                    for (int ran = 0; ran < num_random; ran++)
                    {
                        if (train_data[random_object[ran]] != train_data[random_object[0]])
                        {
                            edist[dcount][0] = EuclideanDistance(train_data[random_object[0]], train_data[random_object[ran]], columnNumber);
                            edist[dcount][1] = random_object[ran];
                            dcount++;
                        }
                    } 

                    /*cout << endl << " Еdist before" << endl;
                    for (int j = 0; j < 2; j++)
                    {
                        for (int j1 = 0; j1 < ed_fact; j1++)
                        {
                            cout << edist[j1][j] << " ";
                        }
                        cout << endl;
                    }*/

                    for(int r = 0; r < ed_fact-1; r++)
                    {
                        // Поиск наименьшего в первом столбце
                        double m = edist[r][0];
                        int idx = r;
                        for(int i = r; i < ed_fact; i++)
                        {
                            if (edist[i][0] < m) 
                            {
                                m = edist[i][0];
                                idx = i;
                                // Обмен
                                swap_rows(edist, r, idx);
                            }
                        }                    
                    }

                    /*cout << endl << " Еdist after" << endl;
                    for (int j = 0; j < 2; j++)
                    {
                        for (int j1 = 0; j1 < ed_fact; j1++)
                        {
                            cout << edist[j1][j] << " ";
                        }
                        cout << endl;
                    }*/

                    obj_for_rule[0] = random_object[0];
                    int count_for_inserting = 0;
                    for (int j = 1; j < num_obj_create_rule; j++)
                    {
                        obj_for_rule[j] = edist[count_for_inserting][1];
                        count_for_inserting++;
                    }

                    //найти ближкий объект другого класса и попробовать не брать его термы
                    //а должны ли все термы быть не такими же, как из другого класса?..
                    create_rule(num_term, columnNumber, train_data, in_rule, obj_for_rule, num_obj_create_rule);

                    //теперь нужно понять насколько хорошо правило, если норм - то оставляем
                    double* in_confid = new double[num_class];
                    for (int j = 0; j < num_class; j++)
                    {
                        in_confid[j] = 0;
                    }
                    
                    confidence(num_class, lineNumber, columnNumber, class_answers, data, in_rule, in_confid);
                    
                    /*cout << endl << " Следующее " << endl;
                    for (int j = 0; j < num_class; j++)
                    {
                        cout << in_confid[j] << " ";
                    }*/

                    /*
                    cout << endl << " Правило " << endl;
                    for (int j = 0; j < columnNumber; j++)
                    {
                        cout << in_rule[j] << " ";
                    }*/

                    double max_confid = 0;
                    int c_confid = 0;
                    for (int j = 0; j < num_class; j++)
                    {
                        if (in_confid[j] > max_confid)
                        {
                            max_confid = in_confid[j];
                            c_confid = j;
                        }
                    }

                    if (answer_class_check == c_confid && max_confid > better_than)
                    { //проверить вот это работу, подумать
                        flag++;
                        for (int y = 0; y < columnNumber; y++)
                        {
                            pop[ipop][q][y] = in_rule[y];
                        }
                        class_rules[0][ipop][q] = c_confid;
                        confid_rules[0][ipop][q] = max_confid;
                        weight_rules[0][ipop][q] = 2*max_confid - 1;
                        active_rules[0][ipop][q] = 1;
                    }
                    else
                    {
                        for (int y = 0; y < columnNumber; y++)
                        {
                            pop[ipop][q][y] = 100;
                        }
                        active_rules[0][ipop][q] = 0;
                    }

                    delete[] in_rule;

                    for (int j = 0; j < ed_fact; j++)
                    {
                        delete edist[j];
                    }
                    delete edist;
                }
                
                //проверка на количество правил
                //если меньше минимального, то дозаполнить
                if (flag < num_class)
                {
                    //сделать по-умному
                    cout << endl << " МЕНЬШЕ МИНИМАЛЬНОГО " << endl;
                }

                /*for (int j = 0; j < number_rules; j++)
                {
                    for (int y = 0; y < columnNumber; y++)
                    {
                        cout << pop[ipop][j][y] << " ";
                    }
                    cout << endl;
                }*/

                delete[] random_object;
                delete[] obj_for_rule;
            }
        }
        
        
        double best_fitness = lineNumber;
        double best_percentage = lineNumber;
        //Старт 
        for (int generation = 0; generation < gen; generation++)
        {
            //cout << "Generation " << generation << endl;
            double** reply_train = new double*[pop_size];
            for (int j = 0; j < pop_size; j++)
            {
                reply_train[j] = new double[train_length];
            }

            double** reply_test = new double*[pop_size];
            for (int j = 0; j < pop_size; j++)
            {
                reply_test[j] = new double[test_length];
            }

            for (int j = 0; j < pop_size; j++)
            {
                for (int l = 0; l < train_length; l++)
                {
                    reply_train[j][l] = 0;
                }
                for (int l = 0; l < test_length; l++)
                {
                    reply_test[j][l] = 0;
                }
            }
            
            
            double error_percentage_train = 0;
            int best_index = -1;
            //отправить на проверку обучающую выборку
            for (int y = 0; y < pop_size; y++)
            {
                for (int j = 0; j < train_length; j++)
                {
                    Rules(train_data[j], pop[y], class_rules[0][y], active_rules[0][y], columnNumber, number_rules, best_rule_for_object_train, reply_train, y, j);//отправка обучающей выборки
                }

                int train_error = 0;
                for (int l = 0; l < train_length; l++)
                {
                    if (reply_train[y][l] != train_class_answers[l])
                    {
                        train_error = train_error + 1;//неправильно классифицированный объекты
                        correct_classification_for_object_train[y][l] = 0;//неправильно классифицированный объект
                    }
                    else
                    {
                        correct_classification_for_object_train[y][l] = 1;//правильно классифицированный объект
                    }
                }
                //количество правильно классиф. объектов для каждого правила
                check_fitness_michegan(correct_classification_for_object_train[y], best_rule_for_object_train[y], y, number_rules, (lineNumber-(cross_num+last_data)), correct_classification_num); 
                
                for (int j = 0; j < number_rules; j++)
                {
                    if (correct_classification_num[y][j] == 0)
                    {
                        active_rules[0][y][j] = 0;
                    }
                }

                int flag_active = 0;
                flag_active = active_rule_flag(active_rules[0][y], number_rules);

                int flag_not_dontcare = 0;
                flag_not_dontcare = dont_care_flag(number_rules, columnNumber, pop[y]);
                
                cout << "train error " << train_error << " out of " << train_length << " with " << flag_active << endl;

                error_percentage_train = double(train_error) / double(train_length);
                //f3 количество не донт care параметров в правиле суммарно по всем правилам 
                fitness_small[y] = error_percentage_train;
                fitness[y] = w1*error_percentage_train + w2*flag_active + w3*flag_not_dontcare;//оптимизировать
                cout << "Train percentage " << error_percentage_train << endl;
                
                if (fitness[y] < best_fitness)
                {
                    best_fitness = fitness[y];
                    best_percentage = error_percentage_train;
                    best_index = y;
                }

                error_matrix(train_length, reply_train[y], train_class_answers, class_value, num_class, class_values_count_train);

            }

            //ввести матрицу ошибок для задач с N классами
            /*
            Нужно отправлять в функцию 
            1) массив ответов для трэйн
            2) массив классов для трэйн
            3) длина
            */

            //реализовать NSGA-II






            //селекция 
                //Пропорциональная-
                //Ранговая нелинейные ранги
                //Турнирная с N размером турнира
            //скрещивание
                //равномерное
                //Новая с кол-вом верно классиф.объектов
            //мутация
                //слабая
                //средняя
                //сильная
//-------------------------------------------удаление массивов для generation----------------------------------------------------

            for (int y = 0; y < pop_size; y++)
            {
                delete reply_test[y];
            }
            delete reply_test;
            
            for (int y = 0; y < pop_size; y++)
            {
                delete reply_train[y];
            }
            delete reply_train;
        }

//-------------------------------------------удаление массивов для kfold----------------------------------------------------
        for (int ip = 0; ip < pop_size; ip++)
        {
            for (int j = 0; j < number_rules; j++)
            {
                delete[] pop[ip][j];
                delete[] pop2[ip][j];
                delete[] pop3[ip][j];
                delete[] out[ip][j];
            }
        }
        for (int ip = 0; ip < pop_size; ip++)
        {
            delete[] pop[ip];
            delete[] pop2[ip];
            delete[] pop3[ip];
            delete[] out[ip];
        }
        delete[] pop;
        delete[] pop2;
        delete[] pop3;
        delete[] out;

        for (int ip = 0; ip < pop_size; ip++)
        {
            delete best_rule_for_object_train[ip];
            delete best_rule_for_object_test[ip];
            delete correct_classification_for_object_train[ip];
            delete fitness_michegan[ip];
            delete correct_classification_num[ip];
        }
        delete best_rule_for_object_train;
        delete best_rule_for_object_test;
        delete correct_classification_for_object_train;
        delete fitness_michegan;
        delete correct_classification_num;

        delete[] class_values_count_train;//остановка
        delete[] fitness;
        delete[] fitness_small;

        for (int ip = 0; ip < npop; ip++)
        {
            for (int j = 0; j < pop_size; j++)
            {
                delete[] confid_rules[ip][j];
                delete[] weight_rules[ip][j];
                delete[] active_rules[ip][j];
                delete[] class_rules[ip][j];
                delete[] rules_update[ip][j];
            }
        }
        for (int ip = 0; ip < npop; ip++)
        {
            delete[] confid_rules[ip];
            delete[] weight_rules[ip];
            delete[] active_rules[ip];
            delete[] class_rules[ip];
            delete[] rules_update[ip];
        }
        delete[] confid_rules;
        delete[] weight_rules;
        delete[] active_rules;
        delete[] class_rules;
        delete[] rules_update;

    }
    
//-------------------------------------------удаление массивов main----------------------------------------------------
    for (int y = 0; y < cross_num_const+last_data; y++)
    {
        delete test_data[y];
    }
    delete test_data;

    for (int y = 0; y < lineNumber-(cross_num_const+last_data); y++)
    {
        delete train_data[y];
    }
    delete train_data;

    delete[] test_class_answers;
    delete[] train_class_answers;

    for (int i = 0; i < lineNumber; i++)
    {
        delete file_mas[i];
        delete data_file[i];
    }
    delete file_mas;
    delete data_file;

    delete[] class_answers;
    delete[] class_value;
    delete[] class_values_count;
    delete[] max;
    delete[] min;

    for (int i = 0; i < columnNumber; i++)
    {
        delete data[i];
    }
    delete data;

    return 0;
}