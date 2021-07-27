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
                sh "ls /root"
                sh "ls /root/quicklisp"
                sh 'sbcl --script jenkins.lisp'
            }
        }

    }
}
