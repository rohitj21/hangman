#include <bits/stdc++.h>
#define mat(t) vector<vector<t>>
using namespace std;
#define n_char 28
vector<int> mem1(n_char, 0);
vector<vector<int>> mem2(n_char, vector<int>(n_char, 0));
vector<vector<vector<int>>> mem3(n_char, vector<vector<int>> (n_char, vector<int>(n_char, 0)));
vector<vector<vector<vector<int>>>> mem4(n_char, vector<vector<vector<int>>> (n_char, vector<vector<int>>(n_char, vector<int>(n_char, 0))));

char find_best_letter(vector<double> & score, vector<bool>& npos){
    int r = -1; double m = -1;
    for (int i = 0; i < n_char; i++){
        if (score[i] > m && !npos[i]){
            m = score[i];
            r = i;
        }
    }
    if(r == -1){
        printf("\n---error---\n");
        throw std::runtime_error("error");
        return 'a';
    }
    return 'a' + r;
}

vector<double> model1(string &word, vector<bool>& npos){
    vector<double> prob(n_char, 0.0);
    double sum = 0;
    for(int i = 0; i<n_char; i++){
        if(!npos[i]){
            prob[i] = mem1[i];
            sum += mem1[i];
        }
        else prob[i] = 0;
    }
    int blanks = 0;
    for(int i =0; i<word.size(); i++){
        if(word[i] == '-'){
            blanks++;
        }
    }
    for(int i = 0; i<n_char; i++){
        if(!npos[i] && sum > 0){
            prob[i] /= sum;
            prob[i] *= blanks;
        }
        
    }
    return prob;
}

vector<double> model2(string &word, vector<bool>& npos){
    string after;
    string before;
    vector<double> score(n_char, 0.0);
    for (int i = 0; i < word.size(); i++)
    {
        if(word[i] != '-'){
            npos[word[i] - 'a'] = true;   
        }
        if(word[i] == '-' && i+1<word.size() && word[i+1] != '-'){
            before.push_back(word[i+1]);
        }
        if(word[i] == '-' && i-1>=0 && word[i-1] != '-'){
            after.push_back(word[i-1]);
        }
    }
    
    for (char ch : before)
    {
        int sum = 0;
        int counts[n_char] = {0};
        for (int i = 0; i < n_char; i++)
        {
            if (!npos[i])
            {
                counts[i] = mem2[i][ch-'a'];
                sum += counts[i];
            }
        }
        for(int i = 0; i<n_char; i++){
            if(counts[i]>0  && sum>0){
                double temp = counts[i];
                score[i] += temp/sum;
            }
        }
    }

    for (char ch : after)
    {
        int sum = 0;
        int counts[n_char] = {0};
        for (int i = 0; i < n_char; i++)
        {
            if (!npos[i]){
                counts[i] = mem2[ch-'a'][i];
                sum += counts[i];
            }
        }
        for(int i = 0; i<n_char; i++){
            if(counts[i]>0 && sum>0){
                double temp = counts[i];
                score[i] += temp/sum;
            }
        }
    }
    return score;

}

vector<double> model3(string &word, vector<bool>& npos){
    vector<double> score(n_char, 0.0);
    pair<string, string> after, between, before;
    for (int i = 0; i < word.size(); i++){
        if(word[i] != '-'){
            npos[word[i] - 'a'] = true;   
        }
        if(word[i] == '-' && i-2 >=0 && word[i-2] != '-' && word[i-1] != '-'){
            after.first.push_back(word[i-2]);
            after.second.push_back(word[i-1]);
        }
        if(word[i] == '-' && i-1 >=0 && i+1<word.size() && word[i-1] != '-' && word[i+1] != '-'){
            between.first.push_back(word[i-1]);
            between.second.push_back(word[i+1]);
        }
        if(word[i] == '-' && i+2<word.size() && word[i+1] != '-' && word[i+2] != '-'){
            before.first.push_back(word[i+1]);
            before.second.push_back(word[i+2]);
        }
    }
    
    for (int i = 0; i<after.first.size(); i++)
    {
        int sum = 0;
        int counts[n_char] = {0};
        int ch1 = after.first[i] - 'a', ch2 = after.second[i]-'a';
        for (int i = 0; i < n_char; i++)
        { 
            if (!npos[i])
            {
                counts[i] = mem3[ch1][ch2][i];
                sum += counts[i];
            }
        }
        for(int i = 0; i<n_char; i++){
            if(counts[i]>0  && sum>0){
                double temp = counts[i];
                score[i] += temp/sum;
            }
        }
    }

    for (int i = 0; i<between.first.size(); i++){
        int sum = 0;
        int counts[n_char] = {0};
        int ch1 = between.first[i] - 'a', ch2 = between.second[i]-'a';
        for (int i = 0; i < n_char; i++)
        { 
            if (!npos[i])
            {
                counts[i] = mem3[ch1][i][ch2];
                sum += counts[i];
            }
        }
        for(int i = 0; i<n_char; i++){
            if(counts[i]>0  && sum>0){
                double temp = counts[i];
                score[i] += temp/sum;
            }
        }
    }
    for (int i = 0; i<before.first.size(); i++){
        int sum = 0;
        int counts[n_char] = {0};
        int ch1 = before.first[i] - 'a', ch2 = before.second[i]-'a';
        for (int i = 0; i < n_char; i++)
        { 
            if (!npos[i])
            {
                counts[i] = mem3[i][ch1][ch2];
                sum += counts[i];
            }
        }
        for(int i = 0; i<n_char; i++){
            if(counts[i]>0  && sum>0){
                double temp = counts[i];
                score[i] += temp/sum;
            }
        }
    }
    return score;
}

vector<double> model4(string &word, vector<bool>& npos){
    vector<double> score(n_char, 0.0);
    vector<string> after(3), between_right(3), between_left(3), before(3);
    for (int i = 0; i < word.size(); i++){
        if(word[i] != '-'){
            npos[word[i] - 'a'] = true;   
        }
        if(word[i] == '-' && i-3 >=0 && word[i-3] != '-' && word[i-2] != '-' && word[i-1] != '-'){
            after[0].push_back(word[i-3]);
            after[1].push_back(word[i-2]);
            after[2].push_back(word[i-1]);
        }
        if(word[i] == '-' && i-2 >=0 && i+1<word.size() && word[i-2] != '-' && word[i-1] != '-' && word[i+1] != '-'){
            between_right[0].push_back(word[i-2]);
            between_right[1].push_back(word[i-1]);
            between_right[2].push_back(word[i+1]);
        }
        if(word[i] == '-' && i-1 >=0 && i+2<word.size() && word[i-1] != '-' && word[i+1] != '-' && word[i+2] != '-' ){
            between_left[0].push_back(word[i-1]);
            between_left[1].push_back(word[i+1]);
            between_left[2].push_back(word[i+2]);
        }
        if(word[i] == '-' && i+3<word.size() && word[i+1] != '-' && word[i+2] != '-' && word[i+3] != '-'){
            before[0].push_back(word[i+1]);
            before[1].push_back(word[i+2]);
            before[2].push_back(word[i+3]);
        }
    }
    
    for (int i = 0; i<after[0].size(); i++)
    {
        int sum = 0;
        int counts[n_char] = {0};
        int ch1 = after[0][i] - 'a', ch2 = after[1][i]-'a', ch3 = after[2][i] - 'a';
        for (int i = 0; i < n_char; i++)
        { 
            if (!npos[i])
            {
                counts[i] = mem4[ch1][ch2][ch3][i];
                sum += counts[i];
            }
        }
        for(int i = 0; i<n_char; i++){
            if(counts[i]>0  && sum>0){
                double temp = counts[i];
                score[i] += temp/sum;
            }
        }
    }

    for (int i = 0; i<between_left[0].size(); i++)
    {
        int sum = 0;
        int counts[n_char] = {0};
        int ch1 = between_left[0][i] - 'a', ch2 = between_left[1][i]-'a', ch3 = between_left[2][i] - 'a';
        for (int i = 0; i < n_char; i++)
        { 
            if (!npos[i])
            {
                counts[i] = mem4[ch1][i][ch2][ch3];
                sum += counts[i];
            }
        }
        for(int i = 0; i<n_char; i++){
            if(counts[i]>0  && sum>0){
                double temp = counts[i];
                score[i] += temp/sum;
            }
        }
    }
    for (int i = 0; i<between_right[0].size(); i++)
    {
        int sum = 0;
        int counts[n_char] = {0};
        int ch1 = between_right[0][i] - 'a', ch2 = between_right[1][i]-'a', ch3 = between_right[2][i] - 'a';
        for (int i = 0; i < n_char; i++)
        { 
            if (!npos[i])
            {
                counts[i] = mem4[ch1][ch2][i][ch3];
                sum += counts[i];
            }
        }
        for(int i = 0; i<n_char; i++){
            if(counts[i]>0  && sum>0){
                double temp = counts[i];
                score[i] += temp/sum;
            }
        }
    }
    for (int i = 0; i<before[0].size(); i++)
    {
        int sum = 0;
        int counts[n_char] = {0};
        int ch1 = before[0][i] - 'a', ch2 = before[1][i]-'a', ch3 = before[2][i] - 'a';
        for (int i = 0; i < n_char; i++)
        { 
            if (!npos[i])
            {
                counts[i] = mem4[i][ch1][ch2][ch3];
                sum += counts[i];
            }
        }
        for(int i = 0; i<n_char; i++){
            if(counts[i]>0  && sum>0){
                double temp = counts[i];
                score[i] += temp/sum;
            }
        }
    }

    return score;
}

char guess(string word, vector<bool>& npos){
    double coeff[] = {1, 1, 2, 10};
    vector<double> m1 = model1(word, npos);
    vector<double> m2 = model2(word, npos);
    vector<double> m3 = model3(word, npos);
    vector<double> m4 = model4(word, npos);
    vector<double> score(n_char, 0);
    for(int i = 0; i<n_char; i++){
        score[i] = m1[i]*coeff[0] + m2[i]*coeff[1] + m3[i] * coeff[2] + m4[i]*coeff[3];
    }
    return find_best_letter(score, npos);
}

int play(string word, bool show_game = false){
    vector<bool> npos(n_char, false);
    string input(word.size(), '-');
    bool done = false;
    int wrong_attempts = 0;
    while(!done){
        char res = guess(input, npos);
        
        if(show_game)
            cout << input << "\t" << res; 

        bool found = false;
        done = true;
        for(int i = 0; i<word.size(); i++){
            if(res == word[i]){
                input[i] = res;
                found = true;
            }
            if(input[i] == '-'){
                done = false;
            }
        }
        if(!found){
            npos[res-'a'] = true;
            wrong_attempts++;
            if(show_game){
                cout << " X";
            }
        }
        if(show_game){
            cout << endl;
        }
    }
    return wrong_attempts;

}
vector<string> create_dictionary(string path){
    ifstream MyFile(path);
    string s;
    vector<string> dictionary;
    for (int i = 0; getline(MyFile, s); i++)
    {
        dictionary.push_back(s);
    }
    MyFile.close();
    return dictionary;
}
void create_model(vector<string> &dictionary){
    // model 1
    for (int i = 0; i < dictionary.size(); i++)
    {
        bool unique_letters[n_char] = {false};
        for (int j = 0; j < dictionary[i].size(); j++)
        {
            unique_letters[dictionary[i][j] -'a'] = true;
        }
        for(int j = 0; j<n_char; j++){
            if(unique_letters[j]){
                mem1[j]++; 
            }
        }
    }
    // model 2
    for (int i = 0; i < dictionary.size(); i++)
    {
        for (int j = 0; j + 1 < dictionary[i].size(); j++)
        {
            int a = dictionary[i][j] - 'a';
            int b = dictionary[i][j+1] - 'a';
            mem2[a][b]++;
        }
    }

    // model3
    for(int i = 0; i<dictionary.size();i++){
        for(int j = 0; j+2 < dictionary[i].size(); j++){
            int a = dictionary[i][j] - 'a';
            int b = dictionary[i][j+1] - 'a';
            int c = dictionary[i][j+2] - 'a';
            mem3[a][b][c]++;
        }
    }
    // model4
    for(int i = 0; i<dictionary.size();i++){
        for(int j = 0; j+3 < dictionary[i].size(); j++){
            int a = dictionary[i][j] - 'a';
            int b = dictionary[i][j+1] - 'a';
            int c = dictionary[i][j+2] - 'a';
            int d = dictionary[i][j+3] - 'a';
            mem4[a][b][c][d]++;
        }
    }
}

pair<vector<string>, vector<string>> train_test_split(vector<string>& dictionary, int test_size){
    vector<bool> in_test_set(dictionary.size(), false);
    pair<vector<string>, vector<string>> res;
    while(test_size){
        int ind = ((rand()%10000)*10000  + rand()%10000) % dictionary.size();
        if(!in_test_set[ind]){
            in_test_set[ind] = true;
            test_size--;
        }
    }
    for(int i = 0; i<dictionary.size(); i++){
        if(in_test_set[i]){
            res.second.push_back(dictionary[i]);
        }
        else res.first.push_back(dictionary[i]);
    }
    return res;
}
int main()
{
    srand(time(NULL));
    vector<string> dictionary = create_dictionary("words_250000_train.txt");
    //cout << play("tobacco", true);

    // Cross Validation
    int n = 10000;
    pair<vector<string>, vector<string>> dict = train_test_split(dictionary, n);
    create_model(dict.first);
    int success = 0;
    for(int i = 0; i<n; i++){
        if(play(dict.second[i]) < 6){
            success++;
        }
        //else cout << play(word, true)<<endl;
    }
    double temp  = success;
    cout << temp/n << endl;
    return 0;
}
