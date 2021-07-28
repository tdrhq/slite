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
                docker {
                    image "fukamachi/roswell:20.01.14.104-alpine"
                }
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
