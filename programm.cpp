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
        diff = pow((x[i] - y[i]), 2);
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
    //int w3 = 1;
    int npop = 4;
    //количество рандомных объектов, по которым провести турнирную селекцию для формирования правила
    int num_random = 4;
    //количество объектов, на основании которых создается правило
    int num_obj_create_rule = 2;
    int kfold = 10;
    int cross_num = lineNumber / kfold;
    int last_data = lineNumber % kfold;
    int cross_num_const = cross_num;
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
        /*double*** confid_rules = new double** [npop];
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
        }*/

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
                int count = 1;
                for (int ran = 0; ran < num_random; ran++)
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
                        random_object[ran + 1] = random_obj;
                    }
                    else 
                    {
                        random_obj = rand() % train_length;
                        while ((train_class_answers[random_obj] != train_class_answers[random_object[0]]))
                        {
                            random_obj = rand() % train_length;
                        }
                        random_object[ran + 1] = random_obj;
                    }
                }

                /*for (int j = 0; j < num_random; j++)
                {
                    cout << random_object[j] << " ";
                }*/

                int ed_fact = 0;
                //сочетание без повторений
                ed_fact = fact(num_random)/(fact(num_obj_create_rule)*fact(num_random-num_obj_create_rule));
                double* edist = new double[ed_fact];
                //обработка массива с выбранными данными из одного класса
                int count = 0;
                int edist_min = 100;
                int num1 = -1;
                int num2 = -1;
                for (int ran = 0; ran < num_random; ran++)
                {
                    for (int ran2 = 0; ran2 < num_random - 1; ran2++)
                    {
                        if (ran != ran2 && ran < ran2)
                        {
                            edist[count] = EuclideanDistance(train_data[random_object[ran]], train_data[random_object[ran2]], columnNumber);
                            count++;
                            //пока затычка на запоминание 2-х чисел
                            if (edist[count] < edist_min)
                            {
                                edist_min = edist[count];
                                num1 = ran;
                                num2 = ran2;
                            }
                        }
                    }
                } 

                obj_for_rule[0] = num1;
                obj_for_rule[1] = num2;

                create_rule(num_term, columnNumber, train_data, in_rule, obj_for_rule, num_obj_create_rule);

                /*
                for (int j = 0; j < columnNumber; j++)
                {
                    cout << in_rule[j] << " ";
                }*/

                delete[] in_rule;
            }
            
            delete[] random_object;//остановка/вопрос
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

        delete[] class_values_count_train;//остановка/вопрос
        delete[] fitness;
        delete[] fitness_small;

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