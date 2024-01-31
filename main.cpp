#include"matrix.h"

using namespace std;



/*****用户参数*****/
int flag=0;//flag=0:frac;flag=1:dec;
int step=0;//中间步骤
/******************/

inline int getint()//过滤非正常字符 安全读数
{
    char ch;
    int p=0;
    while ((ch=getchar())<'0'||ch>'9');
    p*=10;p+=ch-'0';
    while ((ch=getchar())>='0'&&ch<='9') p*=10,p+=ch-'0';
    return p;
}

const int MAXN=105;

class frac//自定义分数类  A/B
{
	protected:
		long long A,B;
	public:
		int sgn(int a){return a<0?-1:1;}//三位运算符
		long long abs(long long a){return a>0?a:-a;}
		int gcd(int a,int b){return a%b?gcd(b,a%b):b;}
		frac(){A=0,B=1;};
		frac(int _A,int _B):A(_A),B(_B){cutdown();}
		frac(int q):A(q),B(1){}
		void init(){A=0,B=1;}
		bool is_cut(){return A*B==0||gcd(A,B)==1;}//是否最简
		void cutdown()//化简分数
		{
			if (B==0) return;
			if (A==0){B=1;return;}
			if (is_cut()) return;
			int r=gcd(abs(A),abs(B));
			A=A/r,B=B/r;
		}
		/****************重载运算符*******************/

		/*****plus*****/
		frac operator + (const frac &p)
		{
			return frac(A*p.B+p.A*B,B*p.B);
		}
		frac operator + (const int &p)
		{
			return (*this)+frac(p);
		}
		friend frac operator + (const int &p,const frac &q)
		{
			return frac(p)+q;
		}

		/*****minus*****/
		frac operator - (const frac &p)
		{
			return frac(A*p.B-p.A*B,B*p.B);
		}
		frac operator - (const int &p)
		{
			return (*this)-frac(p);
		}
		friend frac operator - (const int &p,const frac &q)
		{
			return frac(p)-q;
		}
		/*****mult*****/
		frac operator * (const frac &p)
		{
			return frac(A*p.A,B*p.B);
		}
		frac operator * (const int &p)
		{
			return (*this)*frac(p);
		}
		friend frac operator * (const int &p,const frac &q)
		{
			return frac(p)*q;
		}

		/*****div*****/
		frac operator / (const frac &p)
		{
			return frac(A*p.B,B*p.A);
		}
		frac operator / (const int &p)
		{
			return (*this)/frac(p);
		}
		friend frac operator / (const int &p,const frac &q)
		{
			return frac(p)/q;
		}

		/*****logic*****/
		bool operator ()()
		{
			return (bool)A;
		}
		bool operator !()
		{
			return !(*this)();
		}
		bool operator == (const frac &p)
		{
			return !(*this-p);
		}
		bool operator == (const int &q)
		{
			return !(*this-q);
		}
		friend bool operator == (const int &q,const frac &p)
		{
			return !(q-p);
		}
		bool neg()
		{
			return A*B<0;
		}
		bool operator < (const frac &p)
		{
			return (*this-p).neg();
		}
		bool operator < (const int &p)
		{
			return (*this-p).neg();
		}
		friend bool operator < (const int &q,const frac &p)
		{
			return (q-p).neg();
		}
		/****************IO******************/
		void operator = (const frac &p)
		{
			A=p.A,B=p.B;
			cutdown();
		}
		void operator = (const int &q)
		{
			*this=frac(q);
		}
		void get()
		{
			int q=getint();
			*this=q;
		}
		void output()
		{
			if (!flag)
			{
				cutdown();
				B!=1LL?printf("%lld/%lld",sgn(A*B)*abs(A),abs(B)):printf("%lld",A);
			}
			else printf("%f",1.0*A/B);
		}
}p;

class Matrix
{
	protected:
		int r,c;
		int size,sq;
		frac data[MAXN][MAXN];
	public:
		Matrix():r(0),c(0),size(0),sq(0){}
		Matrix(int _r,int _c):r(_r),c(_c){size=r*c;sq=(r==c);}
		Matrix(int _r,int _c,int k):r(_r),c(_c)
		{
			size=r*c;sq=(r==c);
			for (int i=1;i<=r;i++) data[i][i]=k;
		}
		frac* operator [](int pos){return data[0]+MAXN*pos;};//重载双下标运算符
		/***************矩阵加法****************/
		Matrix* operator + (Matrix M)
		{
			if (r==M.r&&c==M.c)
			{
				Matrix* temp=new Matrix(r,c);
				for (int i=1;i<=r;i++)
				{
					for (int j=1;j<=c;j++)
					{
						(*temp)[i][j]=(*this)[i][j]+M[i][j];
					}
				}
				return temp;
			}

		}
		/***************矩阵减法****************/
		Matrix* operator - (Matrix M)
		{
			if (r==M.r&&c==M.c)
			{
				Matrix* temp=new Matrix(r,c);
				for (int i=1;i<=r;i++)
				{
					for (int j=1;j<=c;j++)
					{
						(*temp)[i][j]=(*this)[i][j]-M[i][j];
					}
				}
				return temp;
			}

		}
		/***************数乘****************/
		Matrix* operator * (int q)
		{
			Matrix* temp=new Matrix(r,c);
			for (int i=1;i<=r;i++)
			{
				for (int j=1;j<=c;j++)
				{
					(*temp)[i][j]=(*this)[i][j]*q;
				}
			}
			return temp;
		}
		friend Matrix* operator * (int q,Matrix &M)
		{
			Matrix* temp=new Matrix(M.r,M.c);
			for (int i=1;i<=M.r;i++)
			{
				for (int j=1;j<=M.c;j++)
				{
					(*temp)[i][j]=M[i][j]*q;
				}
			}
			return temp;
		 }


		/****************矩阵乘法*****************/
        Matrix* operator * (Matrix &M)
		{
			if (c==M.r)
			{
				Matrix* temp=new Matrix(r,M.c);
				for (int i=1;i<=r;i++)
				{
					for (int j=1;j<=M.c;j++)
					{
						for (int k=1;k<=c;k++)
						{
							(*temp)[i][j]=(*temp)[i][j]+(*this)[i][k]*M[k][j];
						}
					}
				}
				return temp;
			}

		}

		/****************初等变换*****************/
		void swap_r(int a,int b)
		{
			for (int i=1;i<=c;i++) swap(data[a][i],data[b][i]);
		}
		void swap_c(int a,int b)
		{
			for (int i=1;i<=r;i++) swap(data[i][a],data[i][b]);
		}
		void mult_r(int _r,frac p)
		{
			for (int i=1;i<=c;i++) data[_r][i]=data[_r][i]*p;
		}
		void mult_c(int _c,frac p)
		{
			for (int i=1;i<=r;i++) data[i][_c]=data[i][_c]*p;
		}
		void add_r(int target,int bullet)
		{
			for (int i=1;i<=c;i++) data[target][i]=data[target][i]+data[bullet][i];
		}
		void add_r(int target,int bullet,frac speed)
		{
			for (int i=1;i<=c;i++) data[target][i]=data[target][i]+data[bullet][i]*speed;
		}
		void add_c(int target,int bullet)
		{
			for (int i=1;i<=r;i++) data[i][target]=data[i][target]+data[i][bullet];
		}
		void add_c(int target,int bullet,frac speed)
		{
			for (int i=1;i<=r;i++) data[i][target]=data[i][target]+data[i][bullet]*speed;

		}

		/*****************行列式求值****************/
		frac det ()
		{
		    ofstream fout;
            string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
            fout.open(miss_Z,ios::app);
			Matrix *temp=new Matrix(r,c,1);//备份原矩阵
			*temp=*this;
			frac D(1);
			if (!sq) return frac(-32767);
			for (int i=1;i<r;i++)
			{
				if (data[i][i]==0)
				{
					for (int j=i+1;j<=r;j++)
					{
						if(!(data[j][i]==0))
						{
							swap_r(i,j);
							D=D*-1;
							if (step)
							{
								printf("----------------------------\n");
								fout<<"----------------------------\n";
								output();
								printf("----------------------------\n");
								fout<<"----------------------------\n";
							}
							break;
						}
					}
				}
				if (data[i][i]==0) return frac(0);
				for (int j=i+1;j<=r;j++)
				{
					add_r(j,i,data[j][i]/data[i][i]*-1);
					if (step)
					{
						printf("----------------------------\n");
						fout<<"----------------------------\n";
						output();
						printf("----------------------------\n");
						fout<<"----------------------------\n";
					}
				}
			}
			for (int i=1;i<=r;i++)
			{
				D=D*data[i][i];
				if (step)
				{
					printf("----------------------------\n");
					fout<<"----------------------------\n";
					output();
					printf("----------------------------\n");
					fout<<"----------------------------\n";
				}
			}
			*this=*temp;//恢复
			return D;
			//基于当前系统的当前时间
    time_t now = time(0);
    cout<< "1970 到目前经过秒数："<< now <<endl;
    fout<< "1970 到目前经过秒数："<< now <<endl;
    tm *ltm = localtime(&now);

    //输出 tm 结构的各个组成部分
    cout<<"年： "  << 1900 + ltm->tm_year <<endl;
    cout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    cout<<"日： "  <<        ltm->tm_mday <<endl;
    cout<<"时间；" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"年： "  << 1900 + ltm->tm_year <<endl;
    fout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    fout<<"日： "  <<        ltm->tm_mday <<endl;
    fout<<"时间；" <<        ltm->tm_hour <<":";
    fout<< ltm->tm_min <<":";
    fout<< ltm->tm_sec <<endl;
    return 0 ;
			fout.close();

		}

		/******************矩阵求逆******************/
		Matrix* rev()
		{
		    ofstream fout;
            string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
            fout.open(miss_Z,ios::app);
			Matrix *ans=new Matrix(r,c,1);//创建矩阵指针ans，指针指向一个矩阵的存储空间，
                                        //其中调用Matrix(r,c,1)的矩阵创建函数。
			Matrix *temp=new Matrix(r,c,1);//指针temp也指向ans一样的矩阵，但存储空间不同
                                        //备份原矩阵
			*temp=*this;//将temp指针指向this指针指向的存储空间。
			for (int i=1;i<=r;i++)
			{
				if (data[i][i]==0)
				{
					for (int j=i+1;j<=r;j++)//初始化一个j循环，从第二行开始，到r行（j随i变）
					{
						if(!(data[j][i]==0))//如果元素（j，i）=0就不执行该if语句
						{
							swap_r(i,j);//调用函数
							ans->swap_r(i,j);//指向行交换矩阵
							if (step)
							{
								printf("----------------------------\n");
								fout<<"----------------------------\n";
								output();//输出行交换矩阵
								putchar('\n');//打印输出换行符
								ans->output();
								printf("----------------------------\n");
								fout<<"----------------------------\n";
							}
							break;
						}
					}
				}
				for (int j=i+1;j<=r;j++)
				{
					ans->add_r(j,i,data[j][i]/data[i][i]*-1);//注意这里有顺序问题！！！
					add_r(j,i,data[j][i]/data[i][i]*-1);//行顺序列顺序迭代
					if (step)
					{
						printf("----------------------------\n");
						fout<<"----------------------------\n";
						output();
						putchar('\n');
						ans->output();
						printf("----------------------------\n");
						fout<<"----------------------------\n";
					}
				}
			}
			for (int i=r;i>1;i--)//与上面是逆序循环
			{
				for (int j=1;j<i;j++)
				{
					ans->add_r(j,i,data[j][i]/data[i][i]*-1);
					add_r(j,i,data[j][i]/data[i][i]*-1);//行倒序列顺序迭代
					if (step)
					{
						printf("----------------------------\n");
						fout<<"----------------------------\n";
						output();
						putchar('\n');
						ans->output();
						printf("----------------------------\n");
						fout<<"----------------------------\n";
					}
				}
			}
			for (int i=1;i<=r;i++)
			{
				ans->mult_r(i,1/data[i][i]);
				mult_r(i,1/data[i][i]);
				if (step)
				{
					printf("----------------------------\n");
					fout<<"----------------------------\n";
					output();
					putchar('\n');
					ans->output();
					printf("----------------------------\n");
					fout<<"----------------------------\n";
				}
			}
			*this=*temp;//恢复
			return ans;
			//基于当前系统的当前时间
    time_t now = time(0);
    cout<< "1970 到目前经过秒数："<< now <<endl;
    tm *ltm = localtime(&now);

    //输出 tm 结构的各个组成部分
    cout<<"年： "  << 1900 + ltm->tm_year <<endl;
    cout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    cout<<"日： "  <<        ltm->tm_mday <<endl;
    cout<<"时间；" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"年： "  << 1900 + ltm->tm_year <<endl;
    fout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    fout<<"日： "  <<        ltm->tm_mday <<endl;
    fout<<"时间；" <<        ltm->tm_hour <<":";
    fout<< ltm->tm_min <<":";
    fout<< ltm->tm_sec <<endl;
    return 0 ;
			fout.close();
		}



		/*******************IO********************/
		void get(int _r,int _c)
		{
			r=_r,c=_c;
			size=r*c;
			sq=r==c;
			for (int i=1;i<=r;i++)
			{
				for (int j=1;j<=c;j++)
				{
					data[i][j].get();
				}
			}
		}
		void output()
		{
			for (int i=1;i<=r;i++)
			{
				for (int j=1;j<=c;j++)
				{
					(*this)[i][j].output();
					putchar(' ');
				}
				putchar('\n');
			}
		}
}M,N;



int pluss()
{
    ofstream fout;
    string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
    fout.open(miss_Z,ios::app);
	int r,c,flag;
	cout<<"#请输入计算类型"<<endl;
	cout<<"1.矩阵加法"<<endl;
	cout<<"2.矩阵减法"<<endl;
	cout<<"0.回到上一级"<<endl;
	fout<<"#请输入计算类型"<<endl;
	fout<<"1.矩阵加法"<<endl;
	fout<<"2.矩阵减法"<<endl;
	fout<<"0.回到上一级"<<endl;
	flag=getint();
	if(!flag) return 0;
	if (flag<0||flag>2)
	{
		cout<<"#输入不合法!!!"<<endl;
		fout<<"#输入不合法!!!"<<endl;
		return 1;
	}

	Matrix* A=new Matrix;
	Matrix* B=new Matrix;
	cout<<"#请输入两个矩阵的行数和列数"<<endl;
	fout<<"#请输入两个矩阵的行数和列数"<<endl;
	cin>>r>>c;
	cout<<"#请输入第一个矩阵,数字之间可用任意非数字字符分隔"<<endl;
	fout<<"#请输入第一个矩阵,数字之间可用任意非数字字符分隔"<<endl;
	A->get(r,c);
	cout<<"#请输入第二个矩阵,数字之间可用任意非数字字符分隔"<<endl;
	fout<<"#请输入第二个矩阵,数字之间可用任意非数字字符分隔"<<endl;
	B->get(r,c);

	cout<<"#计算结果是"<<endl;
	fout<<"#计算结果是"<<endl;
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	if (flag==1) ((*A)+(*B))->output();
	if (flag==2) ((*A)-(*B))->output();
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	cout<<"#谢谢使用!"<<endl;
	return 1;
	//基于当前系统的当前时间
    time_t now = time(0);
    cout<< "1970 到目前经过秒数："<< now <<endl;
    tm *ltm = localtime(&now);

    //输出 tm 结构的各个组成部分
    cout<<"年： "  << 1900 + ltm->tm_year <<endl;
    cout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    cout<<"日： "  <<        ltm->tm_mday <<endl;
    cout<<"时间；" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"年： "  << 1900 + ltm->tm_year <<endl;
    fout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    fout<<"日： "  <<        ltm->tm_mday <<endl;
    fout<<"时间；" <<        ltm->tm_hour <<":";
    fout<< ltm->tm_min <<":";
    fout<< ltm->tm_sec <<endl;
    return 0 ;
	fout.close();
}

int num()
{
    ofstream fout;
    string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
    fout.open(miss_Z,ios::app);
	int r,c,q,flag;
	cout<<"#请输入计算类型"<<endl;
	cout<<"1.矩阵数乘"<<endl;
	cout<<"0.回到上一级"<<endl;
	fout<<"#请输入计算类型"<<endl;
	fout<<"1.矩阵数乘"<<endl;
	fout<<"0.回到上一级"<<endl;
	flag=getint();
	if(!flag) return 0;
	if (flag<0||flag>1)
	{
		cout<<"#输入不合法!!!"<<endl;
		fout<<"#输入不合法!!!"<<endl;
		return 1;
	}

	Matrix* A=new Matrix;
	cout<<"#请输入矩阵的行数和列数"<<endl;
	fout<<"#请输入矩阵的行数和列数"<<endl;
	cin>>r>>c;
	cout<<"#请输入一个数"<<endl;
	fout<<"#请输入一个数"<<endl;
	cin>>q;
	cout<<"#请输入矩阵,数字之间可用任意非数字字符分隔"<<endl;
	fout<<"#请输入矩阵,数字之间可用任意非数字字符分隔"<<endl;
	A->get(r,c);

	cout<<"#计算结果是"<<endl;
	fout<<"#计算结果是"<<endl;
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	((*A) * q)->output();
	putchar('\n');
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	cout<<"#谢谢使用!"<<endl;
	fout<<"#谢谢使用!"<<endl;
	return 1;
	//基于当前系统的当前时间
    time_t now = time(0);
    cout<< "1970 到目前经过秒数："<< now <<endl;
    tm *ltm = localtime(&now);

    //输出 tm 结构的各个组成部分
    cout<<"年： "  << 1900 + ltm->tm_year <<endl;
    cout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    cout<<"日： "  <<        ltm->tm_mday <<endl;
    cout<<"时间；" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"年： "  << 1900 + ltm->tm_year <<endl;
    fout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    fout<<"日： "  <<        ltm->tm_mday <<endl;
    fout<<"时间；" <<        ltm->tm_hour <<":";
    fout<< ltm->tm_min <<":";
    fout<< ltm->tm_sec <<endl;
    return 0 ;
	fout.close();
}
int mul()
{
    ofstream fout;
    string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
    fout.open(miss_Z,ios::app);
	int r1,c1,r2,c2,flag;
	cout<<"#请输入计算类型"<<endl;
	cout<<"1.矩阵乘法"<<endl;
	cout<<"0.回到上一级"<<endl;
	fout<<"#请输入计算类型"<<endl;
	fout<<"1.矩阵乘法"<<endl;
	fout<<"0.回到上一级"<<endl;
	flag=getint();
	if(!flag) return 0;
	if (flag!=1)
	{
		cout<<"#输入不合法!!!"<<endl;
		fout<<"#输入不合法!!!"<<endl;
		return 1;
	}

	Matrix* A=new Matrix;
	Matrix* B=new Matrix;
	cout<<"#请输入第一个矩阵的行数和列数"<<endl;
	fout<<"#请输入第一个矩阵的行数和列数"<<endl;
	cin>>r1>>c1;
	cout<<"#请输入第一个矩阵,数字之间可用任意非数字字符分隔"<<endl;
	fout<<"#请输入第一个矩阵,数字之间可用任意非数字字符分隔"<<endl;
	A->get(r1,c1);
	cout<<"#请输入第二个矩阵的行数和列数"<<endl;
	fout<<"#请输入第二个矩阵的行数和列数"<<endl;
	cin>>r2>>c2;
	if (c1!=r2)
	{
		cout<<"#输入矩阵不可相乘!!!"<<endl;
		fout<<"#输入矩阵不可相乘!!!"<<endl;
		return 1;
	}
	cout<<"#请输入第二个矩阵,数字之间可用任意非数字字符分隔"<<endl;
	B->get(r2,c2);
	cout<<"#计算结果是"<<endl;
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	((*A)*(*B))->output();
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	cout<<"#谢谢使用!"<<endl;
	return 1;
	//基于当前系统的当前时间
    time_t now = time(0);
    cout<< "1970 到目前经过秒数："<< now <<endl;
    tm *ltm = localtime(&now);

    //输出 tm 结构的各个组成部分
    cout<<"年： "  << 1900 + ltm->tm_year <<endl;
    cout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    cout<<"日： "  <<        ltm->tm_mday <<endl;
    cout<<"时间；" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"年： "  << 1900 + ltm->tm_year <<endl;
    fout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    fout<<"日： "  <<        ltm->tm_mday <<endl;
    fout<<"时间；" <<        ltm->tm_hour <<":";
    fout<< ltm->tm_min <<":";
    fout<< ltm->tm_sec <<endl;
    return 0 ;
	fout.close();
}

int Det()
{
    ofstream fout;
    string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
    fout.open(miss_Z,ios::app);
	int r,c,flag;
	cout<<"#请输入计算类型"<<endl;
	cout<<"1.矩阵行列式"<<endl;
	cout<<"0.回到上一级"<<endl;
	fout<<"#请输入计算类型"<<endl;
	fout<<"1.矩阵行列式"<<endl;
	fout<<"0.回到上一级"<<endl;
	flag=getint();
	if(!flag) return 0;
	if (flag<0||flag>1)
	{
		cout<<"#输入不合法!!!"<<endl;
		fout<<"#输入不合法!!!"<<endl;
		return 1;
	}

	Matrix* A=new Matrix;
	cout<<"#请输入矩阵的行数和列数"<<endl;
	fout<<"#请输入矩阵的行数和列数"<<endl;
	cin>>r>>c;
	cout<<"#请输入矩阵,数字之间可用任意非数字字符分隔"<<endl;
	fout<<"#请输入矩阵,数字之间可用任意非数字字符分隔"<<endl;
	A->get(r,c);

	cout<<"#计算结果是"<<endl;
	fout<<"#计算结果是"<<endl;
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	A->det().output();
	putchar('\n');
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	cout<<"#谢谢使用!"<<endl;
	fout<<"#谢谢使用!"<<endl;
	return 1;
	//基于当前系统的当前时间
    time_t now = time(0);
    cout<< "1970 到目前经过秒数："<< now <<endl;
    tm *ltm = localtime(&now);

    //输出 tm 结构的各个组成部分
    cout<<"年： "  << 1900 + ltm->tm_year <<endl;
    cout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    cout<<"日： "  <<        ltm->tm_mday <<endl;
    cout<<"时间；" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"年： "  << 1900 + ltm->tm_year <<endl;
    fout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    fout<<"日： "  <<        ltm->tm_mday <<endl;
    fout<<"时间；" <<        ltm->tm_hour <<":";
    fout<< ltm->tm_min <<":";
    fout<< ltm->tm_sec <<endl;
    return 0 ;
	fout.close();
}

int Rev()
{
    ofstream fout;
    string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
    fout.open(miss_Z,ios::app);
	int r,c,flag;
	cout<<"#请输入计算类型"<<endl;
	cout<<"1.矩阵求逆"<<endl;
	cout<<"0.回到上一级"<<endl;
	fout<<"#请输入计算类型"<<endl;
	fout<<"1.矩阵求逆"<<endl;
	fout<<"0.回到上一级"<<endl;
	flag=getint();
	if(!flag) return 0;
	if (flag<0||flag>1)
	{
		cout<<"#输入不合法!!!"<<endl;
		fout<<"#输入不合法!!!"<<endl;
		return 1;
	}

	Matrix* A=new Matrix;
	Matrix* B=new Matrix;
	cout<<"#请输入矩阵的行数和列数（行数与列数要相同）"<<endl;
	fout<<"#请输入矩阵的行数和列数（行数与列数要相同）"<<endl;
	cin>>r>>c;
	cout<<"#请输入矩阵,数字之间可用任意非数字字符分隔"<<endl;
	fout<<"#请输入矩阵,数字之间可用任意非数字字符分隔"<<endl;
	A->get(r,c);
	int step_temp=step;
	step=0;
	if (A->det()==0)
	{
		cout<<"#行列式为零,输入不合法!!!"<<endl;
		fout<<"#行列式为零,输入不合法!!!"<<endl;
		step=step_temp;
		return 1;
	}
	step=step_temp;
	cout<<"#计算结果是"<<endl;
	fout<<"#计算结果是"<<endl;
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	A->rev()->output();
	putchar('\n');
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	cout<<"#谢谢使用!"<<endl;
	fout<<"#谢谢使用!"<<endl;
	return 1;
	//基于当前系统的当前时间
    time_t now = time(0);
    cout<< "1970 到目前经过秒数："<< now <<endl;
    tm *ltm = localtime(&now);

    //输出 tm 结构的各个组成部分
    cout<<"年： "  << 1900 + ltm->tm_year <<endl;
    cout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    cout<<"日： "  <<        ltm->tm_mday <<endl;
    cout<<"时间；" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"年： "  << 1900 + ltm->tm_year <<endl;
    fout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    fout<<"日： "  <<        ltm->tm_mday <<endl;
    fout<<"时间；" <<        ltm->tm_hour <<":";
    fout<< ltm->tm_min <<":";
    fout<< ltm->tm_sec <<endl;
    return 0 ;
	fout.close();
}

int setting()
{
    ofstream fout;
    string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
    fout.open(miss_Z,ios::app);
	cout<<"#请输入设置类型"<<endl;
	cout<<"1.分数输出"<<endl;
	cout<<"2.小数输出"<<endl;
	cout<<"3.输出中间步骤"<<endl;
	cout<<"4.不输出中间步骤"<<endl;
	cout<<"0.回到上一级"<<endl;
	fout<<"#请输入设置类型"<<endl;
	fout<<"1.分数输出"<<endl;
	fout<<"2.小数输出"<<endl;
	fout<<"3.输出中间步骤"<<endl;
	fout<<"4.不输出中间步骤"<<endl;
	fout<<"0.回到上一级"<<endl;
	int Flag=getint();
	if(!Flag) return 0;
	if (Flag<0||Flag>5)
	{
		cout<<"#输入不合法!!!"<<endl;
		fout<<"#输入不合法!!!"<<endl;
		return 1;
	}
	if (Flag==1) flag=0;
	if (Flag==2) flag=1;
	if (Flag==3) step=1;
	if (Flag==4) step=0;
	cout<<"#设置更新成功"<<endl;
	fout<<"#设置更新成功"<<endl;
	return 1;
	//基于当前系统的当前时间
    time_t now = time(0);
    cout<< "1970 到目前经过秒数："<< now <<endl;
    tm *ltm = localtime(&now);

    //输出 tm 结构的各个组成部分
    cout<<"年： "  << 1900 + ltm->tm_year <<endl;
    cout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    cout<<"日： "  <<        ltm->tm_mday <<endl;
    cout<<"时间；" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"年： "  << 1900 + ltm->tm_year <<endl;
    fout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    fout<<"日： "  <<        ltm->tm_mday <<endl;
    fout<<"时间；" <<        ltm->tm_hour <<":";
    fout<< ltm->tm_min <<":";
    fout<< ltm->tm_sec <<endl;
    return 0 ;
	fout.close();
}

/*******************线性方程组求解*********************/
//解AX=B
//解线性方程组



//函数定义区;
void print_menu()
{
    ofstream fout;
    string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
    fout.open(miss_Z,ios::app);
    system("cls");
    cout<<"------------方程系数和常数矩阵表示如下:\n";
    fout<<"------------方程系数和常数矩阵表示如下:\n";
    for(int j=0;j<lenth;j++)
    {
		cout<<"系数"<<j+1<<'\t';
		fout<<"系数"<<j+1<<'\t';
	}
    cout<<"常数";
    cout<<endl;
    fout<<"常数";
	fout<<endl;
    for(int i=0;i<lenth;i++)
    {
		int j=0;
        for(;j<lenth;j++)
            cout<<setw(8)<<setiosflags(ios::left)<<a[i][j];
        cout<<b[i]<<endl;
        fout<<setw(8)<<setiosflags(ios::left)<<a[i][j];
        fout<<b[i]<<endl;
    }
    //基于当前系统的当前时间
    time_t now = time(0);
    cout<< "1970 到目前经过秒数："<< now <<endl;
    tm *ltm = localtime(&now);

    //输出 tm 结构的各个组成部分
    cout<<"年： "  << 1900 + ltm->tm_year <<endl;
    cout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    cout<<"日： "  <<        ltm->tm_mday <<endl;
    cout<<"时间；" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"年： "  << 1900 + ltm->tm_year <<endl;
    fout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    fout<<"日： "  <<        ltm->tm_mday <<endl;
    fout<<"时间；" <<        ltm->tm_hour <<":";
    fout<< ltm->tm_min <<":";
    fout<< ltm->tm_sec <<endl;
    fout.close();
}

void input()
{
    ofstream fout;
    string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
    fout.open(miss_Z,ios::app);
    int i,j;
    cout<<"方程的个数:";
    fout<<"方程的个数:";
    cin>>lenth;
    if(lenth>Number)
    {
        cout<<"It is too big.\n";
        fout<<"It is too big.\n";
        return;
    }
    x=new char[lenth];
    for(i=0;i<lenth;i++)
        x[i]='a'+i;

    //输入方程矩阵;
    //提示如何输入;
    cout<<"====================================================\n";
    cout<<"请在每个方程里输入"<<lenth<<"系数和一个常数:\n";
    cout<<"例：\n方程:a";
    fout<<"====================================================\n";
    fout<<"请在每个方程里输入"<<lenth<<"系数和一个常数:\n";
    fout<<"例：\n方程:a";
    for(i=1;i<lenth;i++)
    {
        cout<<"+"<<i+1<<x[i];
        fout<<"+"<<i+1<<x[i];
    }
    cout<<"=10\n";
    cout<<"应输入:";
    fout<<"=10\n";
    fout<<"应输入:";
    for(i=0;i<lenth;i++)
        cout<<i+1<<" ";
    cout<<"10\n";
    cout<<"==============================\n";
    fout<<i+1<<" ";
    fout<<"10\n";
    fout<<"==============================\n";


    //输入每个方程;
    for(i=0;i<lenth;i++)
    {
        cout<<"输入方程"<<i+1<<":";
        fout<<"输入方程"<<i+1<<":";
        for(j=0;j<lenth;j++)
            cin>>a[i][j];
        cin>>b[i];
    }

    //备份数据;
    for(i=0;i<lenth;i++)
        for(j=0;j<lenth;j++)
            copy_a[i][j]=a[i][j];
    for(i=0;i<lenth;i++)
        copy_b[i]=b[i];
    copy_lenth=lenth;
    //基于当前系统的当前时间
    time_t now = time(0);
    cout<< "1970 到目前经过秒数："<< now <<endl;
    tm *ltm = localtime(&now);

    //输出 tm 结构的各个组成部分
    cout<<"年： "  << 1900 + ltm->tm_year <<endl;
    cout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    cout<<"日： "  <<        ltm->tm_mday <<endl;
    cout<<"时间；" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"年： "  << 1900 + ltm->tm_year <<endl;
    fout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    fout<<"日： "  <<        ltm->tm_mday <<endl;
    fout<<"时间；" <<        ltm->tm_hour <<":";
    fout<< ltm->tm_min <<":";
    fout<< ltm->tm_sec <<endl;
    fout.close();
}


//高斯列主元排列求解方程;
void gauss_row()
{
    ofstream fout;
    string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
    fout.open(miss_Z,ios::app);
    int i,j;
    //用高斯列主元消区法将系数矩阵变成一个上三角矩阵;
    gauss_row_xiaoqu();

    //打印高斯消去法产生的上三角矩阵;
    for(i=0;i<lenth;i++)
    {
        for(j=0;j<lenth;j++)
        {
            cout<<setw(10)<<setprecision(5)<<a[i][j];
            fout<<setw(10)<<setprecision(5)<<a[i][j];
        }
        cout<<setw(10)<<b[i]<<endl;
        fout<<setw(10)<<b[i]<<endl;
    }
    //通过判断主元位置上的元素是否为零来确定是否有唯一解;
    if(a[lenth-1][lenth-1]!=0)
    {
        cout<<"系数行列式不为零,方程有唯一的解：\n" << endl;
        fout<<"系数行列式不为零,方程有唯一的解：\n" << endl;
        gauss_calculate();
        for(i=0;i<lenth;i++) //输出结果;
        {
            cout<<x[i]<<"="<<b[i]<<"\n";
            fout<<x[i]<<"="<<b[i]<<"\n";
        }
    }
    else
    {
        cout<<"系数行列式等于零,方程没有唯一的解.\n";
        fout<<"系数行列式等于零,方程没有唯一的解.\n";
    }
    //基于当前系统的当前时间
    time_t now = time(0);
    cout<< "1970 到目前经过秒数："<< now <<endl;
    tm *ltm = localtime(&now);

    //输出 tm 结构的各个组成部分
    cout<<"年： "  << 1900 + ltm->tm_year <<endl;
    cout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    cout<<"日： "  <<        ltm->tm_mday <<endl;
    cout<<"时间；" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"年： "  << 1900 + ltm->tm_year <<endl;
    fout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    fout<<"日： "  <<        ltm->tm_mday <<endl;
    fout<<"时间；" <<        ltm->tm_hour <<":";
    fout<< ltm->tm_min <<":";
    fout<< ltm->tm_sec <<endl;
    fout.close();
}

/************************************************************************/
/* 使用高斯消去法，使得系数矩阵变成一个上三角矩阵;
/************************************************************************/
void gauss_row_xiaoqu()
{
    ofstream fout;
    string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
    fout.open(miss_Z,ios::app);
    int i,j,k,maxi;double lik;
    cout<<"用Gauss列主元消去法结果如下:\n";
    fout<<"用Gauss列主元消去法结果如下:\n";
    for(k=0;k<lenth-1;k++)
    {
        maxi = k;
        //寻找主元这一列上最大的元素的行，并与主元所在的行进行交换;
        for(i=k;i<lenth;i++)
        {
            if(a[i][k]>a[maxi][k])
            {
                maxi=i;
            }
        }
        //交换两行数据;
        if(maxi!=k)
        {
            exchange_hang(k,maxi);//
        }

        //消去主元所在这一列剩余的元素;
        for(i=k+1;i<lenth;i++)
        {
            lik=a[i][k]/a[k][k];
            for(j=k;j<lenth;j++)
            {
                a[i][j]=a[i][j]-a[k][j]*lik;
            }
            b[i]=b[i]-b[k]*lik;
        }
    }
    fout.close();
}

void gauss_calculate() //高斯消去法以后计算未知量的结果;
{
    int i,j;double sum_ax;
    b[lenth-1]=b[lenth-1]/a[lenth-1][lenth-1];
    for(i=lenth-2;i>=0;i--)
    {
        for(j=i+1,sum_ax=0;j<lenth;j++)
        {
            sum_ax+=a[i][j]*b[j];
        }
        b[i]=(b[i]-sum_ax)/a[i][i];
    }
}

void exchange_hang(int m,int n) //交换a[][]中和b[]两行;
{
    int j; double temp;
    for(j=0;j<lenth;j++)
    {
        temp=a[m][j];
        a[m][j]=a[n][j];
        a[n][j]=temp;

    }
    temp=b[m];
    b[m]=b[n];
    b[n]=temp;
}


//矩阵求秩

typedef long long ll;
/*变量为n个时，矩阵A大小为n*n，b为等式右边，通常将b设为0，再让A[i][i]+=-1
X保存高斯消元后的结果*/
const double EPS = 1e-8;
typedef vector<double> vec;
typedef vector<vec> mat;

/*O(n^3)*/

vec gauss_jordan(const mat& A, const vec& b)//高斯消元法，
//基本思想是通过将增广矩阵经过行初等变化变成简化阶梯形矩阵
{
    int n = A.size();//size=r*c是一个函数
    mat B(n,vec(n+1)); //Augment Matrix创建一个矩阵函数
    for(int i = 0; i < n; i++)
        for(int j = 0; j < n; j++) B[i][j] = A[i][j];
    for(int i = 0; i < n; i++) B[i][n] = b[i];

    for(int i = 0; i < n; i++){
        int piv = i; //取最大以便判断无解或者无穷多解
        for(int j = i; j < n; j++){
            if(abs(B[j][i]) > abs(B[piv][i]))
                piv = j;//保证piv最大
        }
        if(i != piv) swap(B[i],B[piv]);//若i不等于piv则执行swap(B[i],B[piv])
        if(abs(B[i][i]) < EPS) return vec();//返回上面的矩阵vec()

        //假想把系数变成1，只计算对后面有影响的部分
        for(int j = n; j > i; j--) B[i][j] /= B[i][i];
        for(int j = 0; j < n; j++)
            if(i != j)
            {
            for(int k = i+1; k <= n; k++) B[j][k] -= B[j][i]*B[i][k];
        }
    }
    vec x(n);
    for(int i = 0; i < n; i++) x[i] = B[i][n];
    return x;
}


double determinant(const mat& A)
{
    int n = A.size();
    mat B = A;
    double det = 1;
    int sign = 0;
    for(int i = 0; i < n; i++){
        int piv = i;
        for(int j = i; j < n; j++){
            if(abs(B[j][i]) > abs(B[piv][i])) piv = j;
        }
        if(i != piv) swap(B[i],B[piv]), sign ^= 1;
        if(abs(B[i][i]) < EPS) return 0;
        det *= B[i][i];
        for(int j = i+1; j < n; j++) B[i][j] /= B[i][i];
        for(int j = i+1; j < n; j++) {
            for(int k = i+1; k < n; k++) B[j][k] -= B[j][i]*B[i][k];
        }
    }
    return sign?-det:det;
}

int rank_of_mat(const mat& A)
{
    int n = A.size();
    mat B = A;
    for(int i = 0; i < n; i++){
        int piv = i;
        for(int j = i; j < n; j++){
            if(abs(B[j][i]) > abs(B[piv][i])) piv = j;
        }
        if(i != piv) swap(B[i],B[piv]);
        if(abs(B[i][i]) < EPS) return i;
        for(int j = n; --j > i;) B[i][j] /= B[i][i];
        for(int j = i+1; j < n; j++) {
            for(int k = i+1; k < n; k++) B[j][k] -= B[j][i]*B[i][k];
        }
    }
    return n;
}

double read(){ double t; scanf("%lf",&t); return t; }

//#define LOCAL
int xamind()
{
    ofstream fout;
    string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
    fout.open(miss_Z,ios::app);
#ifdef LOCAL
    freopen("in.txt","r",stdin);
#endif
    vec alpha;
    mat A;
    int n;
    cout<<"输入矩阵的阶数："<<"\n";
    fout<<"输入矩阵的阶数："<<"\n";
    while(cin >>n,n<=0);
    cout<<"输入一个矩阵："<<"\n";
    fout<<"输入一个矩阵："<<"\n";
    A.resize(n);
    for(int i = 0; i < n; i++){
        for(int j = 0; j < n; j++){
            A[i].push_back(read());
        }
    }


    cout<<"\nrank"<<"\n";
    cout<< rank_of_mat(A)<<endl;
    fout<<"\nrank"<<"\n";
    fout<< rank_of_mat(A)<<endl;


    return 0;
    //基于当前系统的当前时间
    time_t now = time(0);
    cout<< "1970 到目前经过秒数："<< now <<endl;
    tm *ltm = localtime(&now);

    //输出 tm 结构的各个组成部分
    cout<<"年： "  << 1900 + ltm->tm_year <<endl;
    cout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    cout<<"日： "  <<        ltm->tm_mday <<endl;
    cout<<"时间；" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"年： "  << 1900 + ltm->tm_year <<endl;
    fout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    fout<<"日： "  <<        ltm->tm_mday <<endl;
    fout<<"时间；" <<        ltm->tm_hour <<":";
    fout<< ltm->tm_min <<":";
    fout<< ltm->tm_sec <<endl;
    return 0 ;
    fout.close();
}


int function_select()
{
    ofstream fout;
    string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
    fout.open(miss_Z,ios::app);
    cout << "\n\t\t\t\t\t请选择你想要进行的矩阵运算:\n\n\n" << endl;
    cout << "\t\t\t\t\t**======================**\n" << endl;
    cout << "\t\t\t\t** 矩阵加减法        ====== 输入  1   **\n" << endl;
    cout << "\t\t\t\t** 矩阵乘法          ====== 输入  2   **\n" << endl;
    cout << "\t\t\t\t** 矩阵数乘          ====== 输入  3   **\n" << endl;
    cout << "\t\t\t\t** 矩阵行列式        ====== 输入  4   **\n" << endl;
    cout << "\t\t\t\t** 矩阵求逆          ====== 输入  5   **\n" << endl;
    cout << "\t\t\t\t** 矩阵求秩          ====== 输入  6   **\n" << endl;
    cout << "\t\t\t\t** 线性方程组求解    ====== 输入  7   **\n" << endl;
    cout << "\t\t\t\t** 矩阵的小秘密      ====== 输入  8   **\n" << endl;
    cout << "\t\t\t\t** 设置              ====== 输入  9   **\n" << endl;
    cout << "\t\t\t\t** 退出              ====== 输入  0   **\n" << endl;
    cout << "\t\t\t\t\t**======================**\n" << endl;
    fout << "\n\t\t\t\t\t请选择你想要进行的矩阵运算:\n\n\n" << endl;
    fout << "\t\t\t\t\t**======================**\n" << endl;
    fout << "\t\t\t\t** 矩阵加减法        ====== 输入  1   **\n" << endl;
    fout << "\t\t\t\t** 矩阵乘法          ====== 输入  2   **\n" << endl;
    fout << "\t\t\t\t** 矩阵数乘          ====== 输入  3   **\n" << endl;
    fout << "\t\t\t\t** 矩阵行列式        ====== 输入  4   **\n" << endl;
    fout << "\t\t\t\t** 矩阵求逆          ====== 输入  5   **\n" << endl;
    fout << "\t\t\t\t** 矩阵求秩          ====== 输入  6   **\n" << endl;
    fout << "\t\t\t\t** 线性方程组求解    ====== 输入  7   **\n" << endl;
    fout << "\t\t\t\t** 矩阵的小秘密      ====== 输入  8   **\n" << endl;
    fout << "\t\t\t\t** 设置              ====== 输入  9   **\n" << endl;
    fout << "\t\t\t\t** 退出              ====== 输入  0   **\n" << endl;
    fout << "\t\t\t\t\t**======================**\n" << endl;


	int select;
	select=getint();
	if (select==1)
	{
		while(pluss());
	}
	else if (select==2)
	{
		while (mul());
	}
	else if (select==3)
	{
		while (num());
	}
	else if (select==4)
	{
		while (Det());
	}
	else if (select==5)
	{
		while (Rev());
	}
	else if (select==6)
	{

           while( xamind());
	}
	else if (select==7)
	{

			input(); //输入方程  ;
            print_menu(); //打印主菜单;

            gauss_row(); //选择解答方式;
	}

	//这里添加其他功能
    else if (select==8)
	{

       ifstream fin;
       string dir = "C:\\Users\\Zada\\Desktop\\M14\\我的秘密.txt";
       fin.open(dir);
       string name1;
       string name2;
       string name3;
       string name4;
       string name5;
       string name6;
       string name7;
       fin >> name1 >> name2>>name3>>name4>>name5>>name6>>name7;
       fin.close();
    cout << "\n\n\n\n";
    cout << name1 << "\n"<<name2 <<"\n"<< name3<<name4<<"\n"<<name5<<"\n"<< name6
    <<"\n"<< name7<<endl;
    cout << "\n\n\n\n";
    return 0;
	}

	else if (select==9)
	{
		while (setting());
	}
	else if (select==0) return 1;
	return 0;
	//基于当前系统的当前时间
    time_t now = time(0);
    cout<< "1970 到目前经过秒数："<< now <<endl;
    tm *ltm = localtime(&now);

    //输出 tm 结构的各个组成部分
    cout<<"年： "  << 1900 + ltm->tm_year <<endl;
    cout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    cout<<"日： "  <<        ltm->tm_mday <<endl;
    cout<<"时间；" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"年： "  << 1900 + ltm->tm_year <<endl;
    fout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    fout<<"日： "  <<        ltm->tm_mday <<endl;
    fout<<"时间；" <<        ltm->tm_hour <<":";
    fout<< ltm->tm_min <<":";
    fout<< ltm->tm_sec <<endl;
    return 0 ;
	fout.close();
}



int main()
{

	while (true)
	{
		if (function_select()) break;
	}
	//基于当前系统的当前时间
    time_t now = time(0);
    cout<< "1970 到目前经过秒数："<< now <<endl;
    tm *ltm = localtime(&now);

    //输出 tm 结构的各个组成部分
    cout<<"年： "  << 1900 + ltm->tm_year <<endl;
    cout<<"月： "  << 1    + ltm->tm_mon  <<endl;
    cout<<"日： "  <<        ltm->tm_mday <<endl;
    cout<<"时间；" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
	return 0;
}
