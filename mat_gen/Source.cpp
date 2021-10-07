#include <iostream>
#include <iomanip>
#include <stdlib.h>
#include <ctime>
#include <vector>
#include <list>
#include <cassert>
#include <numeric>
#include <algorithm>
#include <string> 

using namespace std;

long long gcd(long long a, long long b) {
	while (b) {
		a %= b;
		swap(a, b);
	}
	return a;
}

long long lcm(long long a, long long b) {
	return a / gcd(a, b) * b;
}

long long to_rand(long long min, long long max) {
	return (rand() % (max - min)) + min;
}

struct fraction {
public:

	fraction()
		:_num{ 0 }, _den{ 1 }, _num_sqrt{ 1 }, _den_sqrt{ 1 } {}

	fraction(long long num)
		:_num{ num }, _den{ 1 }, _num_sqrt{ 1 }, _den_sqrt{ 1 } {}

	fraction(long long num, long long den)
		:_num{ num }, _den{ den }, _num_sqrt{ 1 }, _den_sqrt{ 1 } {
		reduce();
	}

	fraction(long long num, long long den, long long num_sqrt, long long den_sqrt)
		:_num{ num }, _den{ den }, _num_sqrt{ num_sqrt }, _den_sqrt{ den_sqrt } {
		reduce();
	}


	long long get_num() const {
		return _num;
	}
	long long const get_den() const {
		return _den;
	}
	long long const get_num_sqrt() const {
		return _num_sqrt;
	}
	long long const get_den_sqrt() const {
		return _den_sqrt;
	}

	bool const check_sqrt() const {
		if (_num_sqrt == 1 and _den_sqrt == 1) return false;
		return true;
	}

	void to_sqrt() {
		_num_sqrt = _num;
		_den_sqrt = _den;
		_num = 1;
		_den = 1;
		reduce();
	}

private:
	long long _num;
	long long _den;
	long long _num_sqrt;
	long long _den_sqrt;

	void factorization() {
		if (_num_sqrt > 1) {
			vector<long long> mul((long long)sqrt(_num_sqrt) + 1, 0);
			for (long long i = 2; i < (long long)sqrt(_num_sqrt) + 1; i++) {
				while (_num_sqrt % i == 0) {
					_num_sqrt /= i;
					mul[i]++;
				}
			}

			for (long long i = 2; i < mul.size(); i++) {
				_num *= (mul[i] / 2 == 0) ? 1 : (mul[i] / 2 * i);
				_num_sqrt *= (mul[i] % 2 == 0) ? 1 : i;
			}
		}
		if (_den_sqrt > 1) {
			vector<long long> mul((long long)sqrt(_den_sqrt) + 1, 0);
			for (long long i = 2; i < (long long)sqrt(_den_sqrt) + 1; i++) {
				while (_den_sqrt % i == 0) {
					_den_sqrt /= i;
					mul[i]++;
				}
			}

			for (long long i = 2; i < mul.size(); i++) {
				_den *= (mul[i] / 2 == 0) ? 1 : (mul[i] / 2 * i);
				_den_sqrt *= (mul[i] % 2 == 0) ? 1 : i;
			}
		}
	}


	void reduce() {
		if (_num == 0) {
			_den = 1;
			_num_sqrt = 1;
			_den_sqrt = 1;
			return;
		}
		if (_num_sqrt == 0) {
			_num = 0;
			_den = 1;
			_num_sqrt = 1;
			_den_sqrt = 1;
			return;
		}
		long long a = gcd(abs(_num), _den);
		_num /= a;
		_den /= a;
		a = gcd(abs(_num_sqrt), _den_sqrt);
		_num_sqrt /= a;
		_den_sqrt /= a;
		factorization();

	}

};

bool operator!=(const fraction& a, const fraction& b) {
	if (a.get_num() != b.get_num()) {
		return true;
	}
	return false;
}

fraction operator+ (const fraction& a, const fraction& b) {

	long long l = lcm(a.get_den(), b.get_den());
	fraction tmp(a.get_num() * (l / a.get_den()) + b.get_num() * (l / b.get_den()), l);

	return tmp;
}

fraction operator- (const fraction& a, const fraction& b) {

	long long l = lcm(a.get_den(), b.get_den());
	fraction tmp(a.get_num() * (l / a.get_den()) - b.get_num() * (l / b.get_den()), l);

	return tmp;
}

fraction operator* (const fraction& a, const fraction& b) {
	fraction tmp(a.get_num() * b.get_num(), a.get_den() * b.get_den(),
		a.get_num_sqrt() * b.get_num_sqrt(), a.get_den_sqrt() * b.get_den_sqrt());
	return tmp;
}

fraction operator/ (const fraction& a, const fraction& b) {
	fraction tmp(a.get_num() * b.get_den(), a.get_den() * b.get_num(),
		a.get_num_sqrt() * b.get_den_sqrt(), a.get_den_sqrt() * b.get_num_sqrt());
	return tmp;
}

ostream& operator<< (ostream& os, fraction a) {
	os << a.get_num() << "/" << a.get_den()
		<< " * sqrt(" << a.get_num_sqrt() << "/" << a.get_den_sqrt() << ")";
	return os;
}

vector<fraction> operator- (const vector<fraction>& a, const vector<fraction>& b) {
	vector<fraction> tmp;
	for (long long i = 0; i < a.size(); i++) {
		tmp.push_back(a[i] - b[i]);
	}

	return tmp;
}

struct matrix {
	matrix(long long n, int seed)
		: N{ n }, mat{ vector<vector<fraction>>(n, vector<fraction>(n)), }
	{
		srand(time(nullptr));
		for (long long i = 0; i < N; i++) {
			long long x = 0;
			for (long long j = 0; j < N; j++) {
				mat[i][j] = to_rand(0, seed) * ((to_rand(0, 1)) ? -1 : 1);
			}
		}
		//print();
		for (long long i = 0; i < N; i++) {
			gram(i);
		}
		//print();
		for (long long i = 0; i < N; i++) {
			normalize(i);
		}
		//print();
		//toPy();
	}

	bool is_mat_suitable() {
		for (long long i = 0; i < N; i++) {
			if (mat[i][i] != fraction(1)) {
				return false;
			}
		}
		return true;
	}

	void print() {
		cout <<endl << N << "x" << N << " Matrix: " << endl;
		cout << "[";
		for (long long i = 0; i < N; i++) {
			cout << "[";
			for (long long j = 0; j < N; j++) {
				cout << mat[i][j];
				if (j == N-1)
				cout << "";
				else 
					cout << ", ";
			}
			if (i == N-1){
			cout << "]";
			}
			else {
				cout << "], ";
				cout << endl;
			}
		}
		cout << "]";
		cout << endl;
	}


	vector<vector<fraction>> transparent_mat() {
		vector<vector<fraction>> a = vector<vector<fraction>>(N, vector<fraction>(N));
		for (long long i = 0; i < N; i++)
		{
			for (long long j = 0; j < N; j++)
			{
				a[j][i] = mat[i][j];
			}
		}
		return a;
	}

	void multiply_mat() {
		vector<vector<fraction>> a = transparent_mat();
		vector<vector<fraction>> c = vector<vector<fraction>>(N, vector<fraction>(N, fraction(0, 1)));
		for (long long i = 0; i < N; i++) {
			for (long long j = 0; j < N; j++) {
				for (long long k = 0; k < N; k++) {
					c[i][j] = c[i][j] + (mat[i][k] * a[k][j]);
				}
			}
		}
		for (long long i = 0; i < N; i++) {
			for (long long j = 0; j < N; j++) {
				mat[i][j] = c[i][j];
			}
		}
	}

	void toPy() {
		for (long long i = 0; i < N; i++) {
			for (long long j = 0; j < N; j++) {
				cout << "[";
				cout << setw(1) << mat[i][j].get_num();
				cout << "/";
				cout << setw(1) << mat[i][j].get_den();
				cout << ", ";
				cout << "math.sqrt(";
				cout << setw(1) << mat[i][j].get_num_sqrt();
				cout << ")/";
				cout << "math.sqrt(";
				cout << setw(1) << mat[i][j].get_den_sqrt();
				cout << ")], ";
			}
		}
	}

private:
	long long N;
	vector<vector<fraction>> mat;



	vector<fraction> get_col(long long k) {
		vector<fraction> tmp;
		for (long long i = 0; i < N; i++) {
			tmp.push_back(mat[i][k]);
		}
		return tmp;
	}

	void put_col(long long k, vector<fraction> tmp) {
		for (long long i = 0; i < N; i++) {
			mat[i][k] = tmp[i];
		}
	}

	fraction scalar(vector<fraction> a, vector<fraction> b) {
		fraction sum = 0;
		for (long long i = 0; i < N; i++) {
			sum = sum + a[i] * b[i];
		}
		return sum;
	}

	vector<fraction> multiply_vec(fraction a, vector<fraction> b) {
		vector<fraction> tmp;
		for (long long i = 0; i < N; i++) {
			tmp.push_back(b[i] * a);
		}
		return tmp;
	}

	vector<fraction> proj(vector<fraction> a, vector<fraction> b) {
		vector<fraction> tmp;
		tmp = multiply_vec(scalar(a, b) / scalar(a, a), a);
		return tmp;
	}

	void gram(long long k) {
		vector<fraction> v = get_col(k);
		for (long long i = 0; i < k; i++) {
			if (scalar(get_col(i), get_col(i)).get_num() == 0) {
				cout << scalar(get_col(i), get_col(i)) << endl;
				cout << "Номер столбца ----- " << k << endl;
			}
			v = v - proj(get_col(i), get_col(k));
		}
		put_col(k, v);
	}

	void normalize(long long k) {
		vector<fraction> tmp = get_col(k);
		fraction a = scalar(tmp, tmp);
		a.to_sqrt();
		tmp = multiply_vec(1 / a, tmp);
		put_col(k, tmp);
	}
};

void generator() {
	cout << "Enter N: " << endl;
	long long N;
	cin >> N;
	cout << "Enter Seed: " << endl;
	int seed;
	cin >> seed;
	while (true) {
		matrix mat(N, seed);
		matrix mat1 = mat;
		mat1.multiply_mat();
		if (mat1.is_mat_suitable()) {
			mat.print();
			break;
		}
	}
	string k;
	cout << endl << "Type anything to restart." << endl;
	cin >> k;
	system("cls");
	generator();
}

int main() {
	generator();
	return 0;
}