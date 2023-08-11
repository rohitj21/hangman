    int n = 1000;
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