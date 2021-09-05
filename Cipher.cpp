#include <iostream>
#include <string>
#include <random>
#include <algorithm>
#include <utility>
#include <vector>
#include <sstream>
#include <cmath>

const long double PI = std::acos(-1.0L);

uint64_t char_to_num(char symbol) {
    if (symbol >= 48 && symbol <= 57)
        return symbol - 48;
    if (symbol >= 65 && symbol <= 90)
        return symbol - 55;
    if (symbol >= 97 && symbol <= 122)
        return symbol - 61;
    if (symbol == 32)
        return 62;
    if (symbol == 46)
        return 63;
    return 64;
}

char num_to_char(uint64_t num) {
    if (num <= 9) {
        return num + 48;
    }
    if (num <= 35) {
        return num + 55;
    }
    if (num <= 61) {
        return num + 61;
    }
    if (num == 62) {
        return 32;
    }
    if (num == 63) {
        return 46;
    }
    return '\n';
}

uint64_t Pow(uint64_t num, uint64_t deg, uint64_t p) {
    if (deg == 0) {
        return 1;
    }
    if (deg % 2 != 0) {
        return (num * Pow(num, deg - 1, p)) % p;
    }
    uint64_t base = Pow(num, deg / 2, p);
    return (base * base) % p;
}

uint64_t BinPow(uint64_t num, uint64_t deg) {
    if (deg == 0) {
        return 1;
    }
    if (deg % 2 != 0) {
        return num * BinPow(num, deg - 1);
    }
    uint64_t base = BinPow(num, deg / 2);
    return base * base;
}

struct BigInt {
    std::vector<uint64_t> nums;
    uint64_t num_system = 10;

    void DeleteZeros() {
        while (!nums.empty() && nums.back() == 0) {
            nums.pop_back();
        }
    }

    BigInt() = default;

    explicit BigInt(std::vector<uint64_t> x, uint64_t s = 10) : nums(std::move(x)), num_system(s) {
    }

    BigInt(const BigInt& other) : nums(other.nums), num_system(other.num_system) {
    }

    explicit BigInt(uint64_t n, uint64_t ns = 10) : num_system(ns) {
        while (n) {
            nums.push_back(n % num_system);
            n /= num_system;
        }
    }

    BigInt& operator=(const BigInt& other) {
        nums = other.nums;
        return *this;
    }

    uint64_t operator[](int i) {
        return nums[i];
    }

    uint64_t operator[](int i) const {
        return nums[i];
    }

    [[nodiscard]] size_t size() const {
        return nums.size();
    }

    BigInt& operator+=(const BigInt& rhs) {
        uint64_t carry = 0, s = std::max(size(), rhs.size()) + 1;
        nums.resize(s);
        for (int i = 0; i < s; ++i) {
            nums[i] += (i < rhs.size() ? rhs[i] : 0) + carry;
            carry = nums[i] / num_system;
            nums[i] %= num_system;
        }
        DeleteZeros();
        return *this;
    }

    BigInt& operator-=(const BigInt& rhs) {

    }

    BigInt& operator+=(uint64_t rhs) {
        *this += BigInt(rhs, num_system);
        return *this;
    }

    BigInt& operator*=(uint64_t rhs) {
        *this = *this * rhs;
        return *this;
    }

    BigInt operator*(uint64_t rhs) {
        uint64_t carry = 0, i = 0;
        std::vector<uint64_t> res;
        while (i < size() || carry) {
            uint64_t now = (i < size() ? nums[i] : 0) * rhs + carry;
            res.push_back(now % num_system);
            carry = now / num_system;
            ++i;
        }
        return BigInt(res, num_system);
    }

    void Print() {
        if (nums.empty()) {
            std::cout << 0 << "\n";
            return;
        }
        for (auto it = nums.rbegin(); it != nums.rend(); ++it) {
            std::cout << *it << " ";
        }
        std::cout << "\n";
    }

    friend BigInt operator+(const BigInt& lhs_, const BigInt& rhs_) {
        BigInt result(lhs_);
        result += rhs_;
        return result;
    }
};

BigInt str_to_bint(std::string num) {
    BigInt number({}, 64);
    number.nums.resize(num.size());
    std::transform(num.begin(), num.end(), number.nums.begin(), char_to_num);
    return number;
};

class CipherP {
private:
    uint64_t p, g, k;

public:
    CipherP() {
        std::cin >> p;
        // std::cin >> g;
        std::cin >> k;
    }

    void Encode(const std::string& message) {
        BigInt number({}, 64);
        number.nums.resize(message.size());
        std::transform(message.begin(), message.end(), number.nums.begin(), char_to_num);
        // number.Print();
        BigInt changed({}, p);
        for (auto it = number.nums.rbegin(); it != number.nums.rend(); ++it) {
            changed *= number.num_system;
            changed += *it;
        }
        changed.DeleteZeros();
        // changed.Print();
        for (auto n : changed.nums) {
            uint64_t b = 1 + rand() % (p - 1);
            // std::cout << b << "---------------------------------------\n";
            std::cout << Pow(g, b, p) << " " << (n * Pow(k, b, p)) % p << "\n";
        }
    }

    std::string Decode() {
        std::string message;
        uint64_t m, r;
        std::vector<uint64_t> nums;
        while (std::cin >> r >> m) {
            uint64_t now = (m * Pow(r, p - k - 1, p)) % p;
            nums.push_back(now);
        }
        BigInt decoded({}, 64);
        for (auto it = nums.rbegin(); it != nums.rend(); ++it) {
            decoded *= p;
            decoded += *it;
        }
        decoded.DeleteZeros();
        for (auto num : decoded.nums) {
            message += num_to_char(num);
        }
        return message;
    }
};

class Fq {
private:
    uint64_t p_;
    std::vector<uint64_t> base_;
    std::vector<uint64_t> coefficients_;

    void NormalizeBase() {
        if (base_.back() == 1) {
            return;
        }
        uint64_t c = Pow(base_.back(), p_ - 2, p_);
        for (auto& a : base_) {
            a = (a * c) % p_;
        }
    }

    void Reduce() {
        int n = static_cast<int>(base_.size() - 1);
        while (coefficients_.size() > n) {
            uint64_t leading = coefficients_.back();
            for (int i = 0; i < n; ++i) {
                coefficients_[coefficients_.size() - i - 2] +=
                        p_ - (leading * base_[n - i - 1]) % p_;
                coefficients_[coefficients_.size() - i - 2] %= p_;
            }
            coefficients_.pop_back();
        }
    }

public:
    Fq() = default;

    Fq(uint64_t _p, std::vector<uint64_t> _coefficients, std::vector<uint64_t> _base)
    : p_(_p)
    , base_(std::move(_base))
    ,coefficients_(std::move(_coefficients)) {
        NormalizeBase();
        Reduce();
    }

    [[nodiscard]] std::vector<uint64_t> GetBase() const {
        return base_;
    }

    [[nodiscard]] const std::vector<uint64_t>& GetCoefficient() const {
        return coefficients_;
    }

    [[nodiscard]] uint64_t GetP() const {
        return p_;
    }

    [[nodiscard]] uint64_t GetDegree() const {
        return coefficients_.size();
    }

    [[nodiscard]] uint64_t GetN() const {
        return base_.size() - 1;
    }

    uint64_t operator[](int i) const {
        return coefficients_[i];
    }

    void Print() {
        for (auto item : coefficients_) {
            std::cout << item << " ";
        }
        std::cout << "\n";
    }
};

Fq operator*(const Fq& lhs, const Fq& rhs) {
    std::vector<uint64_t> base = lhs.GetBase();
    std::vector<uint64_t> coefficients(lhs.GetDegree() + rhs.GetDegree() - 1);
    for (int i = 0; i < lhs.GetDegree(); ++i) {
        for (int j = 0; j < rhs.GetDegree(); ++j) {
            coefficients[i + j] += (lhs[i] * rhs[j]) % lhs.GetP();
            coefficients[i + j] %= lhs.GetP();
        }
    }
    return {lhs.GetP(), coefficients, base};
}

Fq Pow(const Fq& f, uint64_t deg) {
    if (deg == 0) {
        return {f.GetP(), {1}, f.GetBase()};
    }
    if (deg % 2 != 0) {
        return f * Pow(f, deg - 1);
    }
    Fq base = Pow(f, deg / 2);
    return base * base;
}

class CipherQ {
private:
    uint64_t p_;
    Fq poly_;
    Fq key_;

public:
    CipherQ() {
        std::string line;
        std::vector<uint64_t> f;
        std::vector<uint64_t> g;
        std::vector<uint64_t> k;
        {
            std::getline(std::cin, line);
            std::stringstream ss(line);
            ss >> p_;
        }
        {
            std::getline(std::cin, line);
            std::stringstream ss(line);
            int64_t a;
            while (ss >> a) {
                if (a < 0) {
                    a += p_;
                }
                f.push_back(a);
            }
        }
        {
            std::getline(std::cin, line);
            std::stringstream ss(line);
            int64_t a;
            while (ss >> a) {
                if (a < 0) {
                    a += p_;
                }
                g.push_back(a);
            }
        }
        {
            std::getline(std::cin, line);
            std::stringstream ss(line);
            int64_t a;
            while (ss >> a) {
                if (a < 0) {
                    a += p_;
                }
                k.push_back(a);
            }
        }
        poly_ = Fq(p_, g, f);
        key_ = Fq(p_, k, f);
    }

public:
    void Encode(const std::string& message) {
        BigInt number({}, 64);
        number.nums.resize(message.size());
        std::transform(message.begin(), message.end(), number.nums.begin(), char_to_num);
        BigInt changed({}, p_);
        for (auto it = number.nums.rbegin(); it != number.nums.rend(); ++it) {
            changed *= number.num_system;
            changed += *it;
        }
        changed.DeleteZeros();
        uint64_t n = poly_.GetN();
        auto it = changed.nums.begin();
        while (it != changed.nums.end()) {
            auto jt = changed.nums.end();
            if (jt - it > n) {
                jt = it + n;
            }
            uint64_t b = 1 + rand() % (p_ - 1);
            // uint64_t b = 10;
            std::vector<uint64_t> now(it, jt);
            Fq fq(p_, now, poly_.GetBase());
            Pow(poly_, b).Print();
            (fq * Pow(key_, b)).Print();
            it = jt;
        }
    }

    std::string Decode() {
        uint64_t k = key_[0];
        std::string message, line;
        std::vector<uint64_t> nums;
        while (std::getline(std::cin, line)) {
            std::vector<uint64_t> R, M;
            uint64_t a;
            std::stringstream ss(line);
            while (ss >> a) {
                R.push_back(a);
            }
            std::getline(std::cin, line);
            std::stringstream ss1(line);
            while (ss1 >> a) {
                M.push_back(a);
            }
            Fq r(p_, R, key_.GetBase());
            Fq m(p_, M, key_.GetBase());
            Fq now = m * Pow(r, BinPow(p_, key_.GetN()) - k - 1);
            nums.insert(nums.end(), now.GetCoefficient().begin(), now.GetCoefficient().end());
        }
        BigInt decoded({}, 64);
        for (auto it = nums.rbegin(); it != nums.rend(); ++it) {
            decoded *= p_;
            decoded += *it;
        }
        decoded.DeleteZeros();
        for (auto num : decoded.nums) {
            message += num_to_char(num);
        }
        return message;
    }
};

class elliptic_curve {

};

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::mt19937 rand;

    CipherQ cipher;
    std::string m;
    std::getline(std::cin, m);
    cipher.Encode(m);
    return 0;
}
