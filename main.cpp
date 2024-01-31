#include"matrix.h"

using namespace std;



/*****�û�����*****/
int flag=0;//flag=0:frac;flag=1:dec;
int step=0;//�м䲽��
/******************/

inline int getint()//���˷������ַ� ��ȫ����
{
    char ch;
    int p=0;
    while ((ch=getchar())<'0'||ch>'9');
    p*=10;p+=ch-'0';
    while ((ch=getchar())>='0'&&ch<='9') p*=10,p+=ch-'0';
    return p;
}

const int MAXN=105;

class frac//�Զ��������  A/B
{
	protected:
		long long A,B;
	public:
		int sgn(int a){return a<0?-1:1;}//��λ�����
		long long abs(long long a){return a>0?a:-a;}
		int gcd(int a,int b){return a%b?gcd(b,a%b):b;}
		frac(){A=0,B=1;};
		frac(int _A,int _B):A(_A),B(_B){cutdown();}
		frac(int q):A(q),B(1){}
		void init(){A=0,B=1;}
		bool is_cut(){return A*B==0||gcd(A,B)==1;}//�Ƿ����
		void cutdown()//�������
		{
			if (B==0) return;
			if (A==0){B=1;return;}
			if (is_cut()) return;
			int r=gcd(abs(A),abs(B));
			A=A/r,B=B/r;
		}
		/****************���������*******************/

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
		frac* operator [](int pos){return data[0]+MAXN*pos;};//����˫�±������
		/***************����ӷ�****************/
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
		/***************�������****************/
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
		/***************����****************/
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


		/****************����˷�*****************/
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

		/****************���ȱ任*****************/
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

		/*****************����ʽ��ֵ****************/
		frac det ()
		{
		    ofstream fout;
            string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
            fout.open(miss_Z,ios::app);
			Matrix *temp=new Matrix(r,c,1);//����ԭ����
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
			*this=*temp;//�ָ�
			return D;
			//���ڵ�ǰϵͳ�ĵ�ǰʱ��
    time_t now = time(0);
    cout<< "1970 ��Ŀǰ����������"<< now <<endl;
    fout<< "1970 ��Ŀǰ����������"<< now <<endl;
    tm *ltm = localtime(&now);

    //��� tm �ṹ�ĸ�����ɲ���
    cout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    cout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    cout<<"�գ� "  <<        ltm->tm_mday <<endl;
    cout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    fout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    fout<<"�գ� "  <<        ltm->tm_mday <<endl;
    fout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    fout<< ltm->tm_min <<":";
    fout<< ltm->tm_sec <<endl;
    return 0 ;
			fout.close();

		}

		/******************��������******************/
		Matrix* rev()
		{
		    ofstream fout;
            string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
            fout.open(miss_Z,ios::app);
			Matrix *ans=new Matrix(r,c,1);//��������ָ��ans��ָ��ָ��һ������Ĵ洢�ռ䣬
                                        //���е���Matrix(r,c,1)�ľ��󴴽�������
			Matrix *temp=new Matrix(r,c,1);//ָ��tempҲָ��ansһ���ľ��󣬵��洢�ռ䲻ͬ
                                        //����ԭ����
			*temp=*this;//��tempָ��ָ��thisָ��ָ��Ĵ洢�ռ䡣
			for (int i=1;i<=r;i++)
			{
				if (data[i][i]==0)
				{
					for (int j=i+1;j<=r;j++)//��ʼ��һ��jѭ�����ӵڶ��п�ʼ����r�У�j��i�䣩
					{
						if(!(data[j][i]==0))//���Ԫ�أ�j��i��=0�Ͳ�ִ�и�if���
						{
							swap_r(i,j);//���ú���
							ans->swap_r(i,j);//ָ���н�������
							if (step)
							{
								printf("----------------------------\n");
								fout<<"----------------------------\n";
								output();//����н�������
								putchar('\n');//��ӡ������з�
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
					ans->add_r(j,i,data[j][i]/data[i][i]*-1);//ע��������˳�����⣡����
					add_r(j,i,data[j][i]/data[i][i]*-1);//��˳����˳�����
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
			for (int i=r;i>1;i--)//������������ѭ��
			{
				for (int j=1;j<i;j++)
				{
					ans->add_r(j,i,data[j][i]/data[i][i]*-1);
					add_r(j,i,data[j][i]/data[i][i]*-1);//�е�����˳�����
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
			*this=*temp;//�ָ�
			return ans;
			//���ڵ�ǰϵͳ�ĵ�ǰʱ��
    time_t now = time(0);
    cout<< "1970 ��Ŀǰ����������"<< now <<endl;
    tm *ltm = localtime(&now);

    //��� tm �ṹ�ĸ�����ɲ���
    cout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    cout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    cout<<"�գ� "  <<        ltm->tm_mday <<endl;
    cout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    fout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    fout<<"�գ� "  <<        ltm->tm_mday <<endl;
    fout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
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
	cout<<"#�������������"<<endl;
	cout<<"1.����ӷ�"<<endl;
	cout<<"2.�������"<<endl;
	cout<<"0.�ص���һ��"<<endl;
	fout<<"#�������������"<<endl;
	fout<<"1.����ӷ�"<<endl;
	fout<<"2.�������"<<endl;
	fout<<"0.�ص���һ��"<<endl;
	flag=getint();
	if(!flag) return 0;
	if (flag<0||flag>2)
	{
		cout<<"#���벻�Ϸ�!!!"<<endl;
		fout<<"#���벻�Ϸ�!!!"<<endl;
		return 1;
	}

	Matrix* A=new Matrix;
	Matrix* B=new Matrix;
	cout<<"#�������������������������"<<endl;
	fout<<"#�������������������������"<<endl;
	cin>>r>>c;
	cout<<"#�������һ������,����֮���������������ַ��ָ�"<<endl;
	fout<<"#�������һ������,����֮���������������ַ��ָ�"<<endl;
	A->get(r,c);
	cout<<"#������ڶ�������,����֮���������������ַ��ָ�"<<endl;
	fout<<"#������ڶ�������,����֮���������������ַ��ָ�"<<endl;
	B->get(r,c);

	cout<<"#��������"<<endl;
	fout<<"#��������"<<endl;
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	if (flag==1) ((*A)+(*B))->output();
	if (flag==2) ((*A)-(*B))->output();
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	cout<<"#ллʹ��!"<<endl;
	return 1;
	//���ڵ�ǰϵͳ�ĵ�ǰʱ��
    time_t now = time(0);
    cout<< "1970 ��Ŀǰ����������"<< now <<endl;
    tm *ltm = localtime(&now);

    //��� tm �ṹ�ĸ�����ɲ���
    cout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    cout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    cout<<"�գ� "  <<        ltm->tm_mday <<endl;
    cout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    fout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    fout<<"�գ� "  <<        ltm->tm_mday <<endl;
    fout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
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
	cout<<"#�������������"<<endl;
	cout<<"1.��������"<<endl;
	cout<<"0.�ص���һ��"<<endl;
	fout<<"#�������������"<<endl;
	fout<<"1.��������"<<endl;
	fout<<"0.�ص���һ��"<<endl;
	flag=getint();
	if(!flag) return 0;
	if (flag<0||flag>1)
	{
		cout<<"#���벻�Ϸ�!!!"<<endl;
		fout<<"#���벻�Ϸ�!!!"<<endl;
		return 1;
	}

	Matrix* A=new Matrix;
	cout<<"#��������������������"<<endl;
	fout<<"#��������������������"<<endl;
	cin>>r>>c;
	cout<<"#������һ����"<<endl;
	fout<<"#������һ����"<<endl;
	cin>>q;
	cout<<"#���������,����֮���������������ַ��ָ�"<<endl;
	fout<<"#���������,����֮���������������ַ��ָ�"<<endl;
	A->get(r,c);

	cout<<"#��������"<<endl;
	fout<<"#��������"<<endl;
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	((*A) * q)->output();
	putchar('\n');
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	cout<<"#ллʹ��!"<<endl;
	fout<<"#ллʹ��!"<<endl;
	return 1;
	//���ڵ�ǰϵͳ�ĵ�ǰʱ��
    time_t now = time(0);
    cout<< "1970 ��Ŀǰ����������"<< now <<endl;
    tm *ltm = localtime(&now);

    //��� tm �ṹ�ĸ�����ɲ���
    cout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    cout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    cout<<"�գ� "  <<        ltm->tm_mday <<endl;
    cout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    fout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    fout<<"�գ� "  <<        ltm->tm_mday <<endl;
    fout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
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
	cout<<"#�������������"<<endl;
	cout<<"1.����˷�"<<endl;
	cout<<"0.�ص���һ��"<<endl;
	fout<<"#�������������"<<endl;
	fout<<"1.����˷�"<<endl;
	fout<<"0.�ص���һ��"<<endl;
	flag=getint();
	if(!flag) return 0;
	if (flag!=1)
	{
		cout<<"#���벻�Ϸ�!!!"<<endl;
		fout<<"#���벻�Ϸ�!!!"<<endl;
		return 1;
	}

	Matrix* A=new Matrix;
	Matrix* B=new Matrix;
	cout<<"#�������һ�����������������"<<endl;
	fout<<"#�������һ�����������������"<<endl;
	cin>>r1>>c1;
	cout<<"#�������һ������,����֮���������������ַ��ָ�"<<endl;
	fout<<"#�������һ������,����֮���������������ַ��ָ�"<<endl;
	A->get(r1,c1);
	cout<<"#������ڶ������������������"<<endl;
	fout<<"#������ڶ������������������"<<endl;
	cin>>r2>>c2;
	if (c1!=r2)
	{
		cout<<"#������󲻿����!!!"<<endl;
		fout<<"#������󲻿����!!!"<<endl;
		return 1;
	}
	cout<<"#������ڶ�������,����֮���������������ַ��ָ�"<<endl;
	B->get(r2,c2);
	cout<<"#��������"<<endl;
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	((*A)*(*B))->output();
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	cout<<"#ллʹ��!"<<endl;
	return 1;
	//���ڵ�ǰϵͳ�ĵ�ǰʱ��
    time_t now = time(0);
    cout<< "1970 ��Ŀǰ����������"<< now <<endl;
    tm *ltm = localtime(&now);

    //��� tm �ṹ�ĸ�����ɲ���
    cout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    cout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    cout<<"�գ� "  <<        ltm->tm_mday <<endl;
    cout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    fout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    fout<<"�գ� "  <<        ltm->tm_mday <<endl;
    fout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
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
	cout<<"#�������������"<<endl;
	cout<<"1.��������ʽ"<<endl;
	cout<<"0.�ص���һ��"<<endl;
	fout<<"#�������������"<<endl;
	fout<<"1.��������ʽ"<<endl;
	fout<<"0.�ص���һ��"<<endl;
	flag=getint();
	if(!flag) return 0;
	if (flag<0||flag>1)
	{
		cout<<"#���벻�Ϸ�!!!"<<endl;
		fout<<"#���벻�Ϸ�!!!"<<endl;
		return 1;
	}

	Matrix* A=new Matrix;
	cout<<"#��������������������"<<endl;
	fout<<"#��������������������"<<endl;
	cin>>r>>c;
	cout<<"#���������,����֮���������������ַ��ָ�"<<endl;
	fout<<"#���������,����֮���������������ַ��ָ�"<<endl;
	A->get(r,c);

	cout<<"#��������"<<endl;
	fout<<"#��������"<<endl;
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	A->det().output();
	putchar('\n');
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	cout<<"#ллʹ��!"<<endl;
	fout<<"#ллʹ��!"<<endl;
	return 1;
	//���ڵ�ǰϵͳ�ĵ�ǰʱ��
    time_t now = time(0);
    cout<< "1970 ��Ŀǰ����������"<< now <<endl;
    tm *ltm = localtime(&now);

    //��� tm �ṹ�ĸ�����ɲ���
    cout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    cout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    cout<<"�գ� "  <<        ltm->tm_mday <<endl;
    cout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    fout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    fout<<"�գ� "  <<        ltm->tm_mday <<endl;
    fout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
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
	cout<<"#�������������"<<endl;
	cout<<"1.��������"<<endl;
	cout<<"0.�ص���һ��"<<endl;
	fout<<"#�������������"<<endl;
	fout<<"1.��������"<<endl;
	fout<<"0.�ص���һ��"<<endl;
	flag=getint();
	if(!flag) return 0;
	if (flag<0||flag>1)
	{
		cout<<"#���벻�Ϸ�!!!"<<endl;
		fout<<"#���벻�Ϸ�!!!"<<endl;
		return 1;
	}

	Matrix* A=new Matrix;
	Matrix* B=new Matrix;
	cout<<"#��������������������������������Ҫ��ͬ��"<<endl;
	fout<<"#��������������������������������Ҫ��ͬ��"<<endl;
	cin>>r>>c;
	cout<<"#���������,����֮���������������ַ��ָ�"<<endl;
	fout<<"#���������,����֮���������������ַ��ָ�"<<endl;
	A->get(r,c);
	int step_temp=step;
	step=0;
	if (A->det()==0)
	{
		cout<<"#����ʽΪ��,���벻�Ϸ�!!!"<<endl;
		fout<<"#����ʽΪ��,���벻�Ϸ�!!!"<<endl;
		step=step_temp;
		return 1;
	}
	step=step_temp;
	cout<<"#��������"<<endl;
	fout<<"#��������"<<endl;
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	A->rev()->output();
	putchar('\n');
	printf("------------------------------------------------\n");
	fout<<"------------------------------------------------\n";
	cout<<"#ллʹ��!"<<endl;
	fout<<"#ллʹ��!"<<endl;
	return 1;
	//���ڵ�ǰϵͳ�ĵ�ǰʱ��
    time_t now = time(0);
    cout<< "1970 ��Ŀǰ����������"<< now <<endl;
    tm *ltm = localtime(&now);

    //��� tm �ṹ�ĸ�����ɲ���
    cout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    cout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    cout<<"�գ� "  <<        ltm->tm_mday <<endl;
    cout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    fout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    fout<<"�գ� "  <<        ltm->tm_mday <<endl;
    fout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
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
	cout<<"#��������������"<<endl;
	cout<<"1.�������"<<endl;
	cout<<"2.С�����"<<endl;
	cout<<"3.����м䲽��"<<endl;
	cout<<"4.������м䲽��"<<endl;
	cout<<"0.�ص���һ��"<<endl;
	fout<<"#��������������"<<endl;
	fout<<"1.�������"<<endl;
	fout<<"2.С�����"<<endl;
	fout<<"3.����м䲽��"<<endl;
	fout<<"4.������м䲽��"<<endl;
	fout<<"0.�ص���һ��"<<endl;
	int Flag=getint();
	if(!Flag) return 0;
	if (Flag<0||Flag>5)
	{
		cout<<"#���벻�Ϸ�!!!"<<endl;
		fout<<"#���벻�Ϸ�!!!"<<endl;
		return 1;
	}
	if (Flag==1) flag=0;
	if (Flag==2) flag=1;
	if (Flag==3) step=1;
	if (Flag==4) step=0;
	cout<<"#���ø��³ɹ�"<<endl;
	fout<<"#���ø��³ɹ�"<<endl;
	return 1;
	//���ڵ�ǰϵͳ�ĵ�ǰʱ��
    time_t now = time(0);
    cout<< "1970 ��Ŀǰ����������"<< now <<endl;
    tm *ltm = localtime(&now);

    //��� tm �ṹ�ĸ�����ɲ���
    cout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    cout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    cout<<"�գ� "  <<        ltm->tm_mday <<endl;
    cout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    fout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    fout<<"�գ� "  <<        ltm->tm_mday <<endl;
    fout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    fout<< ltm->tm_min <<":";
    fout<< ltm->tm_sec <<endl;
    return 0 ;
	fout.close();
}

/*******************���Է��������*********************/
//��AX=B
//�����Է�����



//����������;
void print_menu()
{
    ofstream fout;
    string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
    fout.open(miss_Z,ios::app);
    system("cls");
    cout<<"------------����ϵ���ͳ��������ʾ����:\n";
    fout<<"------------����ϵ���ͳ��������ʾ����:\n";
    for(int j=0;j<lenth;j++)
    {
		cout<<"ϵ��"<<j+1<<'\t';
		fout<<"ϵ��"<<j+1<<'\t';
	}
    cout<<"����";
    cout<<endl;
    fout<<"����";
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
    //���ڵ�ǰϵͳ�ĵ�ǰʱ��
    time_t now = time(0);
    cout<< "1970 ��Ŀǰ����������"<< now <<endl;
    tm *ltm = localtime(&now);

    //��� tm �ṹ�ĸ�����ɲ���
    cout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    cout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    cout<<"�գ� "  <<        ltm->tm_mday <<endl;
    cout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    fout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    fout<<"�գ� "  <<        ltm->tm_mday <<endl;
    fout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
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
    cout<<"���̵ĸ���:";
    fout<<"���̵ĸ���:";
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

    //���뷽�̾���;
    //��ʾ�������;
    cout<<"====================================================\n";
    cout<<"����ÿ������������"<<lenth<<"ϵ����һ������:\n";
    cout<<"����\n����:a";
    fout<<"====================================================\n";
    fout<<"����ÿ������������"<<lenth<<"ϵ����һ������:\n";
    fout<<"����\n����:a";
    for(i=1;i<lenth;i++)
    {
        cout<<"+"<<i+1<<x[i];
        fout<<"+"<<i+1<<x[i];
    }
    cout<<"=10\n";
    cout<<"Ӧ����:";
    fout<<"=10\n";
    fout<<"Ӧ����:";
    for(i=0;i<lenth;i++)
        cout<<i+1<<" ";
    cout<<"10\n";
    cout<<"==============================\n";
    fout<<i+1<<" ";
    fout<<"10\n";
    fout<<"==============================\n";


    //����ÿ������;
    for(i=0;i<lenth;i++)
    {
        cout<<"���뷽��"<<i+1<<":";
        fout<<"���뷽��"<<i+1<<":";
        for(j=0;j<lenth;j++)
            cin>>a[i][j];
        cin>>b[i];
    }

    //��������;
    for(i=0;i<lenth;i++)
        for(j=0;j<lenth;j++)
            copy_a[i][j]=a[i][j];
    for(i=0;i<lenth;i++)
        copy_b[i]=b[i];
    copy_lenth=lenth;
    //���ڵ�ǰϵͳ�ĵ�ǰʱ��
    time_t now = time(0);
    cout<< "1970 ��Ŀǰ����������"<< now <<endl;
    tm *ltm = localtime(&now);

    //��� tm �ṹ�ĸ�����ɲ���
    cout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    cout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    cout<<"�գ� "  <<        ltm->tm_mday <<endl;
    cout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    fout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    fout<<"�գ� "  <<        ltm->tm_mday <<endl;
    fout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    fout<< ltm->tm_min <<":";
    fout<< ltm->tm_sec <<endl;
    fout.close();
}


//��˹����Ԫ������ⷽ��;
void gauss_row()
{
    ofstream fout;
    string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
    fout.open(miss_Z,ios::app);
    int i,j;
    //�ø�˹����Ԫ��������ϵ��������һ�������Ǿ���;
    gauss_row_xiaoqu();

    //��ӡ��˹��ȥ�������������Ǿ���;
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
    //ͨ���ж���Ԫλ���ϵ�Ԫ���Ƿ�Ϊ����ȷ���Ƿ���Ψһ��;
    if(a[lenth-1][lenth-1]!=0)
    {
        cout<<"ϵ������ʽ��Ϊ��,������Ψһ�Ľ⣺\n" << endl;
        fout<<"ϵ������ʽ��Ϊ��,������Ψһ�Ľ⣺\n" << endl;
        gauss_calculate();
        for(i=0;i<lenth;i++) //������;
        {
            cout<<x[i]<<"="<<b[i]<<"\n";
            fout<<x[i]<<"="<<b[i]<<"\n";
        }
    }
    else
    {
        cout<<"ϵ������ʽ������,����û��Ψһ�Ľ�.\n";
        fout<<"ϵ������ʽ������,����û��Ψһ�Ľ�.\n";
    }
    //���ڵ�ǰϵͳ�ĵ�ǰʱ��
    time_t now = time(0);
    cout<< "1970 ��Ŀǰ����������"<< now <<endl;
    tm *ltm = localtime(&now);

    //��� tm �ṹ�ĸ�����ɲ���
    cout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    cout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    cout<<"�գ� "  <<        ltm->tm_mday <<endl;
    cout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    fout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    fout<<"�գ� "  <<        ltm->tm_mday <<endl;
    fout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    fout<< ltm->tm_min <<":";
    fout<< ltm->tm_sec <<endl;
    fout.close();
}

/************************************************************************/
/* ʹ�ø�˹��ȥ����ʹ��ϵ��������һ�������Ǿ���;
/************************************************************************/
void gauss_row_xiaoqu()
{
    ofstream fout;
    string miss_Z ="C:\\Users\\Zada\\Desktop\\M14\\daily.txt";
    fout.open(miss_Z,ios::app);
    int i,j,k,maxi;double lik;
    cout<<"��Gauss����Ԫ��ȥ���������:\n";
    fout<<"��Gauss����Ԫ��ȥ���������:\n";
    for(k=0;k<lenth-1;k++)
    {
        maxi = k;
        //Ѱ����Ԫ��һ��������Ԫ�ص��У�������Ԫ���ڵ��н��н���;
        for(i=k;i<lenth;i++)
        {
            if(a[i][k]>a[maxi][k])
            {
                maxi=i;
            }
        }
        //������������;
        if(maxi!=k)
        {
            exchange_hang(k,maxi);//
        }

        //��ȥ��Ԫ������һ��ʣ���Ԫ��;
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

void gauss_calculate() //��˹��ȥ���Ժ����δ֪���Ľ��;
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

void exchange_hang(int m,int n) //����a[][]�к�b[]����;
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


//��������

typedef long long ll;
/*����Ϊn��ʱ������A��СΪn*n��bΪ��ʽ�ұߣ�ͨ����b��Ϊ0������A[i][i]+=-1
X�����˹��Ԫ��Ľ��*/
const double EPS = 1e-8;
typedef vector<double> vec;
typedef vector<vec> mat;

/*O(n^3)*/

vec gauss_jordan(const mat& A, const vec& b)//��˹��Ԫ����
//����˼����ͨ����������󾭹��г��ȱ仯��ɼ򻯽����ξ���
{
    int n = A.size();//size=r*c��һ������
    mat B(n,vec(n+1)); //Augment Matrix����һ��������
    for(int i = 0; i < n; i++)
        for(int j = 0; j < n; j++) B[i][j] = A[i][j];
    for(int i = 0; i < n; i++) B[i][n] = b[i];

    for(int i = 0; i < n; i++){
        int piv = i; //ȡ����Ա��ж��޽����������
        for(int j = i; j < n; j++){
            if(abs(B[j][i]) > abs(B[piv][i]))
                piv = j;//��֤piv���
        }
        if(i != piv) swap(B[i],B[piv]);//��i������piv��ִ��swap(B[i],B[piv])
        if(abs(B[i][i]) < EPS) return vec();//��������ľ���vec()

        //�����ϵ�����1��ֻ����Ժ�����Ӱ��Ĳ���
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
    cout<<"�������Ľ�����"<<"\n";
    fout<<"�������Ľ�����"<<"\n";
    while(cin >>n,n<=0);
    cout<<"����һ������"<<"\n";
    fout<<"����һ������"<<"\n";
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
    //���ڵ�ǰϵͳ�ĵ�ǰʱ��
    time_t now = time(0);
    cout<< "1970 ��Ŀǰ����������"<< now <<endl;
    tm *ltm = localtime(&now);

    //��� tm �ṹ�ĸ�����ɲ���
    cout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    cout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    cout<<"�գ� "  <<        ltm->tm_mday <<endl;
    cout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    fout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    fout<<"�գ� "  <<        ltm->tm_mday <<endl;
    fout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
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
    cout << "\n\t\t\t\t\t��ѡ������Ҫ���еľ�������:\n\n\n" << endl;
    cout << "\t\t\t\t\t**======================**\n" << endl;
    cout << "\t\t\t\t** ����Ӽ���        ====== ����  1   **\n" << endl;
    cout << "\t\t\t\t** ����˷�          ====== ����  2   **\n" << endl;
    cout << "\t\t\t\t** ��������          ====== ����  3   **\n" << endl;
    cout << "\t\t\t\t** ��������ʽ        ====== ����  4   **\n" << endl;
    cout << "\t\t\t\t** ��������          ====== ����  5   **\n" << endl;
    cout << "\t\t\t\t** ��������          ====== ����  6   **\n" << endl;
    cout << "\t\t\t\t** ���Է��������    ====== ����  7   **\n" << endl;
    cout << "\t\t\t\t** �����С����      ====== ����  8   **\n" << endl;
    cout << "\t\t\t\t** ����              ====== ����  9   **\n" << endl;
    cout << "\t\t\t\t** �˳�              ====== ����  0   **\n" << endl;
    cout << "\t\t\t\t\t**======================**\n" << endl;
    fout << "\n\t\t\t\t\t��ѡ������Ҫ���еľ�������:\n\n\n" << endl;
    fout << "\t\t\t\t\t**======================**\n" << endl;
    fout << "\t\t\t\t** ����Ӽ���        ====== ����  1   **\n" << endl;
    fout << "\t\t\t\t** ����˷�          ====== ����  2   **\n" << endl;
    fout << "\t\t\t\t** ��������          ====== ����  3   **\n" << endl;
    fout << "\t\t\t\t** ��������ʽ        ====== ����  4   **\n" << endl;
    fout << "\t\t\t\t** ��������          ====== ����  5   **\n" << endl;
    fout << "\t\t\t\t** ��������          ====== ����  6   **\n" << endl;
    fout << "\t\t\t\t** ���Է��������    ====== ����  7   **\n" << endl;
    fout << "\t\t\t\t** �����С����      ====== ����  8   **\n" << endl;
    fout << "\t\t\t\t** ����              ====== ����  9   **\n" << endl;
    fout << "\t\t\t\t** �˳�              ====== ����  0   **\n" << endl;
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

			input(); //���뷽��  ;
            print_menu(); //��ӡ���˵�;

            gauss_row(); //ѡ����ʽ;
	}

	//���������������
    else if (select==8)
	{

       ifstream fin;
       string dir = "C:\\Users\\Zada\\Desktop\\M14\\�ҵ�����.txt";
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
	//���ڵ�ǰϵͳ�ĵ�ǰʱ��
    time_t now = time(0);
    cout<< "1970 ��Ŀǰ����������"<< now <<endl;
    tm *ltm = localtime(&now);

    //��� tm �ṹ�ĸ�����ɲ���
    cout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    cout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    cout<<"�գ� "  <<        ltm->tm_mday <<endl;
    cout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
    fout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    fout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    fout<<"�գ� "  <<        ltm->tm_mday <<endl;
    fout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
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
	//���ڵ�ǰϵͳ�ĵ�ǰʱ��
    time_t now = time(0);
    cout<< "1970 ��Ŀǰ����������"<< now <<endl;
    tm *ltm = localtime(&now);

    //��� tm �ṹ�ĸ�����ɲ���
    cout<<"�꣺ "  << 1900 + ltm->tm_year <<endl;
    cout<<"�£� "  << 1    + ltm->tm_mon  <<endl;
    cout<<"�գ� "  <<        ltm->tm_mday <<endl;
    cout<<"ʱ�䣻" <<        ltm->tm_hour <<":";
    cout<< ltm->tm_min <<":";
    cout<< ltm->tm_sec <<endl;
	return 0;
}
