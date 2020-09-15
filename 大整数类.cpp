

#include <iostream>
#include <iomanip>
#include<string>
#include<sstream>
#include<vector>
#include <deque>
#include<algorithm>
#include<fstream>
using namespace std;
class BigInt {
	friend ostream& operator<<(ostream& os, const BigInt& x);
	friend istream& operator>>(istream& is, BigInt& x);
	deque<uint32_t> number;
	static const uint32_t LIMIT = 1000000000;

	pair<BigInt, uint32_t>divide(const uint32_t& b)const {
		BigInt res;
		uint64_t carry = 0;
		if (b == 0) { cout << "输入错误 除或者取余0" << endl; throw "DivideByZero"; }
		for (auto ap = this->number.begin(); ap != this->number.end(); ++ap) {
			if (b > * ap&& carry == 0) {//有余数必然大
				carry = *ap;
				if (!res.number.empty())res.number.push_back(0);//商0
				continue;
			}
			else
			{
				carry *= LIMIT;
				carry += *ap;
				res.number.push_back(carry / b);//直接除法计算商
				carry %= b;//取余计算余数
			}
		}
		if (res.number.empty())res = { 0 };//防止BigInt<b因为条件无法判断没写BigInt<uint32_t的运算符
		return make_pair(res, carry);
	}
	pair<BigInt, BigInt> divide(const BigInt& b) const {
		BigInt res;
		BigInt carry;
		if (b.number.front() == 0) { cout << "输入错误 除或者取余0" << endl; throw "DivideByZero"; }
		for (auto ap = this->number.begin(); ap != this->number.end(); ++ap) {
			carry.number.push_back(*ap);
			if (carry < b) {
				if (!res.number.empty())res.number.push_back(0);//商0
				while (!carry.number.empty() && carry.number.front() == 0)carry.number.pop_front();
				continue;
			}
			else if (b < carry) {
				uint32_t l = 1, r = LIMIT - 1, m;
				BigInt cur, next;
				while (1)
				{
					m = (l + r) / 2;
					cur = { b * m };
					next = { cur + b };
					if (cur < carry && carry < next)break;
					if (cur < carry)
						l = m + 1;
					else if (carry < cur)
						r = m - 1;
					else break;
				}
				res.number.push_back(m);
				carry -= cur;
				while (!carry.number.empty()&&carry.number.front() == 0)carry.number.pop_front();
			}
			else {//b=carry
				res.number.push_back(1);
				carry.number.clear();
			}
		}
		if (carry.number.empty())carry = { 0 };
		if (res.number.empty())res = { 0 };
		return make_pair(res, carry);
	}
public:

	BigInt() {}
	BigInt(const string& s) {
		stringstream sstr;
		string ss;
		ss.reserve(s.size() * 2);//预留足够出空间防止反复扩容
		size_t i = s.size();
		while (i >= 9) {//每隔九个数字加一个空格
			i -= 9;
			ss += s.substr(i, 9);//从低位往高位9位一组分割写入
			ss.push_back(' ');
		}
		if (i)ss += s.substr(0, i);
		sstr << ss;
		uint64_t temp;
		while (sstr >> temp)
			number.push_front(temp);
		while (number.size() > 1 && number.front() == 0)//处理字符串的最前多余的0
			number.pop_front();
	}
	BigInt(initializer_list<uint32_t> list) {
		number.assign(list);
	}
	BigInt(deque<uint32_t>::const_iterator beg, deque<uint32_t>::const_iterator end) {
		number.assign(beg, end);
	}
	~BigInt() {}
	BigInt operator+(const BigInt& b) const {
		BigInt res;
		auto ap = this->number.rbegin();
		auto bp = b.number.rbegin();
		uint32_t s = 0;
		uint32_t carry = 0;
		while (ap != this->number.rend() || bp != b.number.rend())
		{
			if (ap == this->number.rend()) {//左操作数已经到结尾
				s = *bp + carry;
				carry = s / (LIMIT);//处理溢出10^9的进位
				res.number.push_front(s % LIMIT);//计算结果本位
				++bp;
			}
			else if (bp == b.number.rend()) {//右操作数已经到结尾
				s = *ap + carry;
				carry = s / (LIMIT);
				res.number.push_front(s % LIMIT);
				++ap;
			}
			else {
				s = *bp + *ap + carry;
				carry = s / (LIMIT);
				res.number.push_front(s % LIMIT);
				++ap;
				++bp;
			}
		}
		if (carry)res.number.push_front(carry);//处理最后的进位
		return res;
	}
	BigInt& operator+=(const BigInt& b) {
		return *this = *this + b;
	}
	BigInt operator*(const uint32_t& b) const {
		BigInt res;//最前方为最高位，最高位为0则BigInt为0
		if (this->number.front() == 0 || b == 0)
			return res = { 0 };
		uint32_t carry = 0;
		uint64_t s;//使用64位防止溢出
		for (auto ap = this->number.rbegin(); ap != this->number.rend(); ++ap) {
			s = (uint64_t)*ap * b + carry;
			carry = s / LIMIT;
			res.number.push_front(s % LIMIT);
		}
		if (carry)res.number.push_front(carry);
		return res;
	}
	BigInt& operator*=(const uint32_t& b) {
		return *this = *this * b;
	}
	BigInt operator*(const BigInt& b) const {
		BigInt res;
		if (this->number.front() == 0 || b.number.front() == 0)
			return res = { 0 };
		int shift = 0;//移位计数
		for (auto bp = b.number.crbegin(); bp != b.number.crend(); ++bp) {
			BigInt temp;
			temp = *this * *bp;//调用上面的BigInt*uint32_t
			if (temp.number.front())//BigInt不为0
				for (int i = 0; i < shift; ++i)
					temp.number.push_back(0);
			++shift;
			res += temp;
		}
		return res;
	}
	BigInt& operator*=(const BigInt& b) {
		return *this = *this * b;
	}

	BigInt operator-(const BigInt& b) const {//由调用者保证左边大于右边
		BigInt res;
		if (*this < b) { cout << "输入错误 小数减大数" << endl; throw "MinusOverFlow"; }
		if (*this == b) return res = { 0 };
		auto ap = this->number.rbegin();
		auto bp = b.number.rbegin();
		uint32_t s = 0;
		uint32_t carry = 0;
		while (ap != this->number.rend() || bp != b.number.rend())
		{
			if (bp == b.number.rend()) {//只可能右边先结束
				if (*ap >= carry) {//不用借位
					s = *ap - carry;
					carry = 0;
				}
				else
				{
					s = *ap - carry + LIMIT;
					carry = 1;
				}
				++ap;
			}
			else {
				if (*ap >= (*bp + carry)) {//不用借位
					s = *ap - *bp - carry;
					carry = 0;
				}
				else {
					s = *ap - *bp - carry + LIMIT;
					carry = 1;
				}
				++ap;
				++bp;
			}
			if (s)res.number.push_front(s);
		}
		return res;
	}
	BigInt& operator-=(const BigInt& b) {
		return *this = *this - b;
	}
	BigInt operator/(const uint32_t& b)const {
		return divide(b).first;
	}
	BigInt& operator/=(const uint32_t b) {
		return *this = *this / b;
	}
	BigInt operator/(const BigInt& b)const {
		return divide(b).first;
	}
	BigInt& operator/=(const BigInt& b) {
		return *this = *this / b;
	}
	uint32_t operator%(const uint32_t& b)const {
		return divide(b).second;
	}
	BigInt& operator%=(const uint32_t& b) {
		return *this = BigInt({ *this % b });
	}
	BigInt operator%(const BigInt& b) const {
		return divide(b).second;
	}
	BigInt& operator%=(const BigInt& b) {
		return *this = *this % b;
	}
	BigInt& operator^=(const uint32_t& b) {
		if (b == 1)return *this;
		if (b & 1) {
			BigInt a = *this;
			*this *= *this;
			*this ^= b / 2;
			*this *= a;
		}
		else {
			*this *= *this;
			*this ^= b / 2;
		}
		return *this;
	}
	BigInt operator^(const uint32_t& b)const {
		BigInt res = { *this };
		return res ^= b;
	}
	bool operator<(const BigInt& b) const {
		if (this->number.size() < b.number.size())
			return true;
		else if (this->number.size() == b.number.size() && this->number < b.number)
			return true;
		else return false;
	}
	bool operator>(const BigInt& b)const {
		return b < *this;
	}
	bool operator==(const BigInt& b)const {
		return !(*this < b || *this > b);
	}
	bool operator>=(const BigInt& b)const {
		return !(*this < b);
	}
	bool operator<=(const BigInt& b)const {
		return !(*this > b);
	}
};
ostream& operator<<(ostream& os, const BigInt& x) {
	for (auto p = x.number.begin(); p != x.number.end(); ++p) {
		if (p != x.number.begin())os << setw(9) << setfill('0');
		os << *p;
	}
	return os;
}
istream& operator>>(istream& is, BigInt& x) {
	string temp;
	is >> temp;
	x = { temp };
	return is;
}

vector<BigInt> a;
vector<BigInt> u = { {"314882150829468584"} ,//2^3*3947*1455859*6849701
						{"427197303358170108"} ,//2^2*3*157*373*607909278869
						{"1022292690726729920" },//2^6*5*1109*366901*7851359
						{"1698479428772363217" },//3*227*1171*2129885634067
						{"2006101093849356424" }//2^3*6115411*41005034123
};
#define DEBUG
int main()
{

#ifdef DEBUG
	fstream cin("input.txt");
#endif // DEBUG
	BigInt a("100000000000000000"), b ("1");
	unsigned long long LL= 0;
	for (int i = 70; i > 0; i--) {
		cout << "REAL:" << b << endl;
		cout << "BI  :" << b % BigInt("18446744073709551616") << endl;//64位int极限;
		cout << "LL  :" << LL << endl;
		LL <<= 1;
		b *= 2;
	}
	cout << (BigInt("1111111111111111111111111111111111111111")^64) << endl;
}
