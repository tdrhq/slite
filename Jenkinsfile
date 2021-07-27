pipeline {
    stages {
        stage ("checkout") {
            steps {
                checkout([
                    $class: 'GitSCM',
                    branches: [[name: "**/master" ]],
                    userRemoteConfigs: [[url: 'https://github.com/tdrhq/slite.git']]
                ])
            }
        }

        stage("Run tests") {
            steps {
                sh "true"
            }
        }

    }
}
