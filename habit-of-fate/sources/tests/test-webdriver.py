#!/usr/bin/env python
import time
import unittest

from pyvirtualdisplay import Display

from selenium import webdriver

from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

class Tests(unittest.TestCase):

    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(10)

    def tearDown(self):
        self.driver.close()

    def wait_for_element_by_id(self, element_id):
        return WebDriverWait(self.driver, 10).until(
            EC.presence_of_element_located((By.ID, element_id))
        )

    def donttest_creating_with_blank_username_returns_error_message(self):
        driver = self.driver
        driver.get('http://localhost:8081/create')
        driver.find_element_by_id('create-account').click()
        self.assertEqual(
            'No username was provided.',
            self.wait_for_element_by_id('error-message').text
        )

    def donttest_creating_with_blank_password_returns_error_message(self):
        driver = self.driver
        driver.get('http://localhost:8081/create')
        driver.find_element_by_name('username').send_keys('username')
        driver.find_element_by_id('create-account').click()
        self.assertEqual(
            'No password was provided.',
            driver.find_element_by_id('error-message').text
        )

    def donttest_creating_with_blank_password2_returns_error_message(self):
        driver = self.driver
        driver.get('http://localhost:8081/create')
        driver.find_element_by_name('username').send_keys('username')
        driver.find_element_by_name('password').send_keys('password')
        driver.find_element_by_id('create-account').click()
        self.assertEqual(
            'You need to repeat the password.',
            driver.find_element_by_id('error-message').text
        )

    def donttest_creating_with_mismatched_passwords_returns_error_message(self):
        driver = self.driver
        driver.get('http://localhost:8081/create')
        driver.find_element_by_name('username').send_keys('username')
        driver.find_element_by_name('password').send_keys('password')
        driver.find_element_by_name('password2').send_keys('password2')
        driver.find_element_by_id('create-account').click()
        self.assertEqual(
            'The passwords do not match.',
            driver.find_element_by_id('error-message').text
        )

    def test_creating_with_conflicting_account_returns_error_message(self):
        driver = self.driver
        driver.get('http://localhost:8081/create')
        driver.find_element_by_name('username').send_keys('username')
        driver.find_element_by_name('password').send_keys('password')
        driver.find_element_by_name('password2').send_keys('password')
        driver.find_element_by_id('create-account').click()
        driver.get('http://localhost:8081/create')
        driver.find_element_by_name('username').send_keys('username')
        driver.find_element_by_name('password').send_keys('different')
        driver.find_element_by_name('password2').send_keys('different')
        driver.find_element_by_id('create-account').click()
        print(driver.page_source)
        self.assertEqual(
            'The account already exists.',
            driver.find_element_by_id('error-message').text
        )


#with Display(visible=0, size=(800, 600)):
if True:
    unittest.main()
