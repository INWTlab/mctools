// Author: Michelle Golchert
pipeline {
    agent none
    options { disableConcurrentBuilds() }
    environment {
        CUR_PROJ = 'mctools' // github repo name
        CUR_PKG = 'mctools' // r-package name
        CUR_PKG_FOLDER = '.' // defaults to root
        INWT_REPO = 'inwt-vmdocker1.inwt.de:8081'
        EMAIL = 'andreas.neudecker@inwt-statistics.de'
    }
    stages {
        stage('Testing with R') {
            agent { label 'test' }
            when { not { branch 'depl' } }
            environment {
                TMP_SUFFIX = """${sh(returnStdout: true, script: 'echo `cat /dev/urandom | tr -dc \'a-z\' | fold -w 6 | head -n 1`')}"""
            }
            steps {
                sh '''
                docker build --pull -t tmp-$CUR_PROJ-$TMP_SUFFIX .
                docker run --rm --network host tmp-$CUR_PROJ-$TMP_SUFFIX check
                docker rmi tmp-$CUR_PROJ-$TMP_SUFFIX
                '''
            }
        }
        stage('Deploy R-package') {
            agent { label 'eh2' }
            when { branch 'master' }
            steps {
                sh '''
                rm -vf *.tar.gz
                docker pull inwt/r-batch:latest
                docker run --rm --network host -v $PWD:/app --user `id -u`:`id -g` inwt/r-batch:latest R CMD build $CUR_PKG_FOLDER
                R -e "drat::insertPackage('`echo $CUR_PKG`_`grep -E '^Version:[ \t]*[0-9.]{3,10}' $CUR_PKG_FOLDER/DESCRIPTION | awk '{print $2}'`.tar.gz', '/var/www/html/r-repo'); drat::archivePackages(repopath = '/var/www/html/r-repo')"
                '''
            }
        }
        stage('Deploy Docker-image') {
            agent { label 'docker' }
            when  { branch 'master' }
            steps {
                sh '''
                docker build --pull -t $INWT_REPO/$CUR_PROJ:latest .
                docker push $INWT_REPO/$CUR_PROJ:latest
                docker rmi $INWT_REPO/$CUR_PROJ:latest
                '''
            }
        }
    }
    post {
        failure {
           script {
                if (env.BRANCH_NAME == 'master' || env.BRANCH_NAME == 'depl') {
                    emailext (
                        attachLog: true,
                        body: "Build of job ${env.JOB_NAME} (No. ${env.BUILD_NUMBER}) has completed\n\nBuild status: ${currentBuild.currentResult}\n\n${env.BUILD_URL}\n\nSee attached log file for more details of the build process.",
                        recipientProviders: [[$class: 'DevelopersRecipientProvider'], [$class: 'RequesterRecipientProvider']],
                        to: "${env.EMAIL}",
                        subject: "Jenkins Build ${currentBuild.currentResult}: Job ${env.JOB_NAME}"
                    )
                }
            }
        }
    }
}
