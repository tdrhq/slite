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
                sh "echo $USER"
                sh "pwd"
                sh 'sbcl --script jenkins.lisp'
            }
        }

    }
}
