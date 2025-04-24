#include <cmath>
#include <vector>
#include <numeric>
#include <limits>
#include <string>
#include <unordered_map>
#include <unordered_set>

/**
 * Computes the entropy of a discrete sequence.
 * @param vec Input vector containing discrete values.
 * @param base Logarithm base (default: 10).
 * @param NA_rm If true, removes NaN values; otherwise returns NaN if any NaN exists.
 * @return Entropy value or NaN if invalid conditions occur.
 */
double CppEntropy_Disc(const std::vector<double>& vec,
                       double base = 10, bool NA_rm = false) {
  std::unordered_map<double, int> counts;
  int valid_n = 0;

  for (double x : vec) {
    if (std::isnan(x)) {
      if (!NA_rm) return std::numeric_limits<double>::quiet_NaN();
      continue;
    }
    counts[x]++;
    valid_n++;
  }

  if (valid_n == 0) return std::numeric_limits<double>::quiet_NaN();

  const double log_base = std::log(base);
  double entropy = 0.0;

  for (const auto& pair : counts) {
    double p = static_cast<double>(pair.second) / valid_n;
    entropy += p * std::log(p);
  }

  return -entropy / log_base;
}

/**
 * Computes the joint entropy of a multivariate discrete sequence.
 * @param mat Input matrix where each row represents a sample containing multiple variables.
 * @param columns The columns which used in joint entropy estimation.
 * @param base Logarithm base (default: 10).
 * @param NA_rm If true, removes samples with any NaN; otherwise returns NaN if any NaN exists.
 * @return Joint entropy value or NaN if invalid conditions occur.
 */
double CppJoinEntropy_Disc(const std::vector<std::vector<double>>& mat,
                           const std::vector<int>& columns,
                           double base = 10, bool NA_rm = false){
  const double log_base = std::log(base);

  // Flattened and valid samples, stored as string key or unique encoding
  std::unordered_map<std::string, int> counts;
  int valid_count = 0;

  for (const auto& sample : mat) {
    bool has_nan = false;
    std::string key;

    for (size_t i = 0; i < columns.size(); ++i) {
      double val = sample[columns[i]];
      if (std::isnan(val)) {
        has_nan = true;
        break;
      }
      // simple separator-based encoding
      key += std::to_string(val) + "_";
    }

    if (has_nan) {
      if (!NA_rm) return std::numeric_limits<double>::quiet_NaN();
      continue;
    }

    counts[key]++;
    valid_count++;
  }

  if (valid_count == 0) return std::numeric_limits<double>::quiet_NaN();

  double entropy = 0.0;
  for (const auto& pair : counts) {
    double p = static_cast<double>(pair.second) / valid_count;
    entropy += p * std::log(p);
  }

  return -entropy / log_base;
}

/**
 * Computes the mutual information between two sets of discrete variables (columns).
 * @param mat Input matrix where each row represents a sample and each column a discrete variable.
 * @param columns1 Indices of columns representing the first set of variables (X).
 * @param columns2 Indices of columns representing the second set of variables (Y).
 * @param base Logarithm base used in entropy calculations (default: 10).
 * @param NA_rm If true, removes samples with any NaN values; otherwise returns NaN if any NaN is encountered.
 * @return Mutual information value I(X; Y) = H(X) + H(Y) - H(X,Y), or NaN if invalid conditions occur.
 */
double CppMutualInformation_Disc(const std::vector<std::vector<double>>& mat,
                                 const std::vector<int>& columns1,
                                 const std::vector<int>& columns2,
                                 double base = 10, bool NA_rm = false) {
  std::unordered_set<int> unique_set;
  unique_set.insert(columns1.begin(), columns1.end());
  unique_set.insert(columns2.begin(), columns2.end());
  std::vector<int> columns(unique_set.begin(), unique_set.end());

  double h_x = CppJoinEntropy_Disc(mat, columns1, base, NA_rm);
  double h_y = CppJoinEntropy_Disc(mat, columns2, base, NA_rm);
  double h_xy = CppJoinEntropy_Disc(mat, columns, base, NA_rm);

  if (std::isnan(h_x) || std::isnan(h_y) || std::isnan(h_xy)) {
    return std::numeric_limits<double>::quiet_NaN();
  }

  return h_x + h_y - h_xy;
}

/**
 * Computes the conditional entropy H(X | Y) between two sets of discrete variables.
 * @param mat Input matrix where each row is a sample and each column is a discrete variable.
 * @param target_columns Indices of columns representing the target variable(s) X.
 * @param conditional_columns Indices of columns representing the conditioning variable(s) Y.
 * @param base Logarithm base used in entropy calculations (default: 10).
 * @param NA_rm If true, removes samples with any NaN values; otherwise returns NaN if any NaN is encountered.
 * @return Conditional entropy value H(X | Y) = H(X,Y) - H(Y), or NaN if invalid conditions occur.
 */
double CppConditionalEntropy_Disc(const std::vector<std::vector<double>>& mat,
                                  const std::vector<int>& target_columns,
                                  const std::vector<int>& conditional_columns,
                                  double base = 10, bool NA_rm = false) {
  std::unordered_set<int> unique_set;
  unique_set.insert(target_columns.begin(), target_columns.end());
  unique_set.insert(conditional_columns.begin(), conditional_columns.end());
  std::vector<int> columns(unique_set.begin(), unique_set.end());

  double H_xy = CppJoinEntropy_Disc(mat, columns, base, NA_rm);
  double H_y = CppJoinEntropy_Disc(mat, conditional_columns, base, NA_rm);

  if (std::isnan(H_xy) || std::isnan(H_y)) {
    return std::numeric_limits<double>::quiet_NaN();
  }

  return H_xy - H_y; // H(X|Y) = H(X,Y) - H(Y)
}
