pipeline {
    agent any

    stages {
        stage ("checkout") {
            steps {
                checkout([
                    $class: 'GitSCM',
                    branches: [[name: "*/main" ]],
                    userRemoteConfigs: [[url: 'https://github.com/tdrhq/slite.git']]
                ])
            }
        }

        stage("Run tests") {
            agent {
                dockerfile true
            }

            steps {
                sh "env"
                sh "pwd"
                sh "ls -l"
                sh "ros install sbcl"
                sh 'sbcl --script jenkins.lisp'
            }
        }

    }
}
